unit module X11;

use X11::XCB;
use X11::XCB::XProto;

# Workaround for Promise::Vow invisibility.  Needs RT
my constant Vow = Promise.new.vow.WHAT;

our class AuthInfo is export {
    has xcb_auth_info_t $.xcb handles <<name namelen data datalen>>;
    method BUILD (:$name, :$namelen, :$data, :$datalen) {
        $!xcb = xcb_auth_info_t.new;
        $!xcb.name = $name;
        $!xcb.data = $data;
        $!xcb.namelen = $namelen with $namelen;
        $!xcb.datalen = $datalen with $datalen;
    }
}

my constant @ganr = buf16.new(0x1717,2), buf32.new(4); # abuse pad for endian

our class X::X11::NoReply is Exception {
    method message { "No Reply" }
}

# XXX seems not to work here, but works below Connection... ?!?
#our class Window is export {...}
#our class Resource is export {...}

our class Connection is export {
    has xcb_connection_t $.xcb;

    #| A map of how many times this connection has subscribed to events
    #| from various windows.  Keyed by low level window id, containing
    #| BagHashes which are keyed by the event number.
    has BagHash %.evmasks;

    has $.Setup;

    has uint32 $!screen = 0;
    has xcb_query_extension_reply_t $!ext; # should never be freed
    has Channel $.cookies = Channel.new();
    has Channel $.destroying = Channel.new();
    has Channel $!watch = Channel.new();

    # For now we store this per-connection.  How to tie all this
    # mess together threadsafely and safe to various monkeying TBD.
    has $.extmap;
    #| Extension base value followed by Error code to type maps from
    #| said extension, pre-sorted by descending base value.
    has $.error_bases is rw;
    has $.event_bases is rw;
    has $.xge_bases is rw;
    #| Lock for preventing simultaneous access to error/event bases
    #| (May be used in future for runtime extension loading)
    has Lock $.error_lock = Lock.new();
    has Lock $.event_lock = Lock.new();
    has Lock $.xge_lock = Lock.new();
    #| Lock used *only* by requests that send fds, to prevent
    #| disordering between calls to xcb_send_fd and xcb_send_request
    has Lock $.fd_lock = Lock.new();

    method flush { xcb_flush($!xcb) }

    #| Change the Channel to which unowned events are sent.
    #| Returns the new Channel.  The first value, before any
    #| events, sent on the new Channel, will be the old Channel
    #| that was just replaced, or if none, False.
    method watch(Channel:D $w = Channel.new --> Channel) {
        $!watch.send($w);
        $w;
    }

    #| Stop watching this connection for unowned events.
    #| If there was a Channel to which events were being sent,
    #| it is closed.
    method unwatch(--> Nil) {
        $!watch.send(False);
    }

    method Setup handles<protocol_major_version protocol_minor_version
                         release_number resource_id_base resource_id_mask
                         motion_buffer_size maximum_request_length
                         image_byte_order bitmap_format_bit_order
                         bitmap_format_scanline_unit bitmap_format_scanline_pad
                         min_keycode max_keycode vendor
                         pixmap_formats roots> {
        use NativeCall;
        return $!Setup if $!Setup.defined;
        $!Setup = Setup.new(nativecast(Pointer, xcb_get_setup($.xcb)), :!free);
    }

    # Passing these as parameters owns their GC link so we can destroy
    # the parent object gracefuly.
    my sub resource_server(Channel $scrap, Channel $ask,
                           xcb_connection_t $c is raw --> Channel) {
        my @scrapheap;
        my Channel $res = Channel.new;

        start { CATCH { $_.note }; react {
            whenever $scrap {
                @scrapheap.push: $_;
                LAST { @scrapheap = (); }
                QUIT { @scrapheap = (); }
            }
            whenever $ask {
                my $r = +@scrapheap ?? @scrapheap.shift !! xcb_generate_id($c);
                $res.send($r);
                LAST { $res.close; }
                QUIT { $res.fail; }
            }
        }};

        # XXX Activate both whenevers to build their phaser scopes.  RT#129761.
        $ask.send(1);
        $scrap.send($res.receive);

        $res;
    }
    has $.res_scrap = Channel.new;
    has $.res_ask = Channel.new;
    has $.res_chute = resource_server($!res_scrap, $!res_ask, $!xcb);

    # Passing these as parameters owns their GC link so we can destroy
    # the parent gracefuly.
    my sub wait(Channel $cookies, Channel $watch, Channel $destroying,
                xcb_connection_t $xcb is raw,
                $error_bases, $error_lock,
                $event_bases, $event_lock,
		$xge_bases, $xge_lock) {

        use NativeCall;
        constant $null = Pointer.new(0);

        # Use xcb handle as a sentry value.
        my $sentry = nativecast(Pointer, $xcb);
        my $fd = xcb_get_file_descriptor($xcb);
        # TODO allow airtight initialization of $watcher
        my Channel $watcher; # Where to send unowned events
        my %errors;

        my sub destroy($r?) {
            my $msg = "Connection has been destroyed";
            while $cookies.poll -> $v {
                $v.break(Failure.new($msg))
                    if $v ~~ Vow; # XXX
                $v.fail($msg) if $v ~~ Channel;
            };
            # XXX race
            $cookies.close;
	    $r.fail($msg) if $r;
            while $watch.poll -> $v {
                $v.break(Failure.new($msg))
                    if $v ~~ Vow; # XXX
                $v.fail($msg) if $v ~~ Channel;
            };
            # XXX race
            $watch.close;
            if $watcher.defined {
                $watcher.fail($msg);
            }
            $destroying.close;
            xcb_disconnect($xcb);
        }

        my sub error_to_exception($err, $req) {
            return Nil unless $err.defined;
            my $left = Inf;
            my $res = RequestError.subclass(nativecast(Pointer,$err),
                                            :$error_bases, :$error_lock,
                                            :$left, :free);

            my class stub is repr('CStruct') {
                has uint8 $.response_type is rw;
                has uint8 $.error_code is rw;
                has uint16 $.sequence is rw;
                has uint32 $.bad_value;
            }

            my $major_opcode = $res.?major_opcode // "TODO";
            my $minor_opcode = $res.?minor_opcode // $req.?opcode // "TODO";
            my $bad_value = $res.?bad_value;

            if not $bad_value.defined and ($res.cstruct.wiresize >= 8) {
                $bad_value = nativecast(stub, $err).bad_value
            };
            $bad_value //= "TODO";

            use X::Protocol::X11;

            X::Protocol::X11.new(:status($res.error_code || 1), # TODO, need X::Protocol subclasses
                                 :sequence($res.sequence),
                                 :$major_opcode,
                                 :$minor_opcode,
                                 :$bad_value
                                 # Need to add this to X::Protocol::X11
                                 #,:$origin($res);
                                 );
        }

        start { CATCH { $_.note };
            my Channel $responses; # channel we are sending responses on
            my Promise $sent;      # thread handling cookie we are working on
            my Promise $evsent = start { }; # thread that handled last event
            my $kick = 0;          # keepalive counter for void requests

            loop {
                # For now just treat the two channels the same.
                # Later we may restrict what values we take on each.
                my sub running_poll { $watch.poll orelse $cookies.poll }

                # Check for shutdown
                if $destroying.poll {
                    destroy($sent);
                    last
                }
                xcb_select_r($fd, 100000); # XXX adjust 100000
                # Were we shutdown while waiting?
                if $destroying.poll {
                    destroy($sent);
                    last
                }
                without $_ {
                    $kick = 0;
                    $_ = running_poll;
                }
                with $_ {
                    if $_ === False {
                        $kick = 0;
                        # Turn off events (discard them)
                        $watcher.close if ($watcher.defined);
                        $watcher = Nil;
                        $_ = Nil;
                        next;
                    }
                    elsif $_ ~~ Channel {
                        $kick = 0;
                        # Change the channel where unowned events are queued
                        # Send the old one back as first value, for chaining
                        $_.send($watcher.defined ?? $watcher !! False);
                        $watcher = $_;
                        $_ = Nil;
                        next;
                    }
                    elsif $_ !~~ Vow {
                        $kick = 0;
                        "Only send Cookie Vows, Channels, or False here".note;
                        $_ = Nil;
                        next;
                    }
                    else {
                        # Do a GetAtomName on 4(ATOM) to jog the pipe.
                        if $kick++ > 10 {
                            GetAtomNameRequest.xcb_send_request($xcb,@ganr);
                            $kick = 0;
                        }
                    }
                }

                my Pointer $e = $sentry.clone;
                my Pointer $r = $sentry.clone;

                xcb_flush($xcb);

                # This must always be called before xcb_poll_for_reply or
                # xcb_poll_for_reply will not work. :-/
                while xcb_poll_for_event($xcb) -> $ev {
                    # This is generic enough to use for errors, too.
                    my $gev = nativecast(X11::XCB::Event::cstruct, $ev);
                    if $gev.code eq 0 {
                        # An error.  Despite what the API docs say it
                        # can indeed be for a reply we requested without the
                        # checked flag set, maybe even this one.  Add it to
                        # the list, which is protected via the whenever.
                        %errors{$gev.sequence} = $ev;
                    }
                    elsif ($gev.code +& 0x7f) > 1 {
                        # An event.  If we have a watcher, use it.
                        if $watcher.defined {

                            my class eventstub does X11::XCB::Event[0] {
                                method cstruct { X11::XCB::Event::cstruct };
                            }
                            my class xgestub does X11::XCB::XGEvent[0] {
                                method cstruct { X11::XCB::XGEvent::cstruct };
                            }
                            my $code = $gev.code;

                            $evsent .= then({
                                my $left = Inf;
                                my $res;
                                if $code == 35 {
                                     $res = xgestub.subclass(
                                        nativecast(Pointer,$ev), :$left, :free,
                                        :$xge_bases, :$xge_lock);
                                }
                                else {
                                     $res = eventstub.subclass(
                                        nativecast(Pointer,$ev), :$left, :free,
                                        :$event_bases, :$event_lock);
                                }
                                $watcher.send($res)
                            });
                        }
                        else {
                            X11::XCB::xcb_free($ev);
                        }
                    }
                    else {
                        "primary code 128 or 129... do what now?".note;
                        X11::XCB::xcb_free($ev);
                    }
                }

                without $_ {
                    $_ = running_poll;
                    next;
                }

                my $status = xcb_poll_for_reply($xcb, .promise.sequence, $r, $e);
                if $status {
                    $kick = 0;

                    # Definitive answer
                    if $r == $sentry or $e == $sentry {
                        die("BUG: xcb_poll_for_reply returned 1 without" ~
                            " altering both reply and error.\n" ~
                            "BUG: This should be impossible.  API changed?");
                    }
                    if $e !== $null {
                        # This will probably never happen given the proclivity
                        # of this API to send all events above.
                        if ($sent.defined) {
                            my $c = $responses;
                            my $e2 = $e;
                            my $p = $_;
                            $sent.then({
                                my $ev = error_to_exception($e2,
                                                     $p.promise.reply_type);
                                $c.fail($ev === Any ?? $e2 !! $ev)
                            });
                        }
                        else {
                            my $p = $_;
                            my $e2 = $e;
                            start {
                                my $ev = error_to_exception($e2,
                                                     $p.promise.reply_type);
                                $p.break($ev === Any ?? $e2 !! $ev)
                            }
                        }
                        next;
                    }
                    elsif $r == $null {
                        # Definitively no more results.
                        if ($sent.defined) {
                            my $ev = %errors{.promise.sequence +& 0xffff}:delete;
                            my $c = $responses;
                            my $p = $_;
                            $sent.then({
                                $ev = error_to_exception($ev,
                                                         $p.promise.reply_type);
                                $ev === Any ?? $c.close !! $c.fail($ev);
                            })
                        }
                        else {
                            my $ev = %errors{.promise.sequence +& 0xffff}:delete;
                            my $p = $_;
                            start {
                                $ev = error_to_exception($ev,
                                                     $p.promise.reply_type);
                                $p.break($ev === Any ?? X::X11::NoReply.new
                                                     !! $ev);
                            }
                        };
                        $sent = Nil;
                        $_ = running_poll;
                        next;
                    }
                    else {
                        if ($sent.defined) {
                            my $c = $responses;
                            my $p = $_;
                            my $r2 = $r;
                            # TODO can fds happen in multi-replies?
                            $sent .= then({
                                $c.send: $p.promise.reply_type.new(
                                    $r2, :left(Int), :free
                                );
                            });
                        }
                        else {
                            # We could just build a list but a Channel allows
                            # multiple workers to respond.  Don't know if
                            # there are any actual use cases for this, though.
                            $responses = Channel.new;
                            my $c = $responses;
                            my $p = $_;
                            my $r2 = $r;
                            my $fds;
                            # This actually just returns an
                            # address where the fds are appended
                            # after the packet data.  But the interior "API"
                            # doesn't promise that, nor can we assume
                            # this call ignores the state of $xcb.
                            # So, we cannot do this in a thread
                            # It implicitly promises no need to free it,
                            # at least.
                            my $rt = $p.promise.reply_type;
                            if $rt.does(X11::XCB::HasFD) {
                                $fds = xcb_get_reply_fds($xcb, $r,
                                    $rt.cstruct.wiresize);
                            }
                            $sent = start {
                                $p.keep($c);
                                $c.send: $rt.new(
                                    $r2, :left(Int), :free, :$fds
                                );
                                CATCH { $_.note }
                            }
                        }
                    }
                }
                elsif $r !== $sentry or $e !== $sentry {
                    die("BUG: xcb_poll_for_reply returned 0 while" ~
                        " altering either the reply or the error.\n" ~
                        "BUG: This should be impossible.  API changed?");
                }
                # This is a pretty messed up interface.  The response when a
                # reply is pending and the response when all replies have
                # been fully read are the same.  But, the input queue does
                # not publish the replies for a request until they are all
                # received so we'll never get blocking while pending in the
                # middle of a string of replies.  We can just use state to
                # figure out what happened.
                elsif ($sent.defined) {
                    my $c = $responses;
                    $sent.then({$c.close});
                    $sent = Nil;
                    $_ = running_poll;
                }
            }
        }
    }
    has $.waiter = wait($!cookies, $!watch, $!destroying, $!xcb,
                        $!error_bases, $!error_lock,
                        $!event_bases, $!event_lock,
                        $!xge_bases, $!xge_lock);

    method DESTROY {
        $!res_ask.close;
        $!res_scrap.close;
        $!destroying.send(True);
    }

    my sub ext_init($xcb) {
        my $extmap = Hash[Any,Any].new(
            ::X11::XCB::.keys.map({
                my $name = $_;
                with ::("X11::XCB::$_\::\&extension_t") {
                    |($_(), $name)
                } 
            })
        );
        my @errorcodes;
        my @eventcodes;
        my @xgecodes;
        for $extmap.kv -> $extt, $modname {
            my $reply = xcb_get_extension_data($xcb, $extt);
            if $reply.present {
                @errorcodes.push(
                    Pair.new($reply.first_error +& 0xff,
                             ::("X11::XCB::$modname\::\$errorcodes")))
                    if $reply.first_error;
                @eventcodes.push(
                    Pair.new($reply.first_event +& 0xff,
                             ::("X11::XCB::$modname\::\$eventcodes")))
                    if $reply.first_event;
                @xgecodes.push(
                    Pair.new($reply.major_opcode +& 0xff,
                             ::("X11::XCB::$modname\::\$eventcodes")))
                    if $reply.major_opcode;
            }
        }
        @errorcodes.push(Pair.new(0, $X11::XCB::XProto::errorcodes));
        @eventcodes.push(Pair.new(0, $X11::XCB::XProto::eventcodes));
        @xgecodes.push(Pair.new(0, $X11::XCB::XProto::xgecodes));

        @errorcodes .= sort: *.key;
        @eventcodes .= sort: *.key;
        @xgecodes .= sort: *.key;

        $extmap,
        [@errorcodes.reverse.map({|($_.key,$_.value)})],
        [@eventcodes.reverse.map({|($_.key,$_.value)})],
        [@xgecodes.reverse.map({|($_.key,$_.value)})];
    }

    multi method new (Int $fd!, X11::AuthInfo :$Auth) {
        my $xcb = xcb_connect_to_fd($fd, $Auth);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }
        my $extmap;
        my $error_bases;
        my $event_bases;
        my $xge_bases;
	($extmap, $error_bases, $event_bases, $xge_bases) = ext_init($xcb);
        self.bless(:$xcb, :$extmap, :$error_bases, :$event_bases, :$xge_bases);
    }
    multi method new (IO $io!, X11::AuthInfo :$Auth) {
        fail "Getting the fd from {$io.perl} NYI"
            unless $io.can("native-descriptor");
        my $fd = $io.native-descriptor;
        self.new($fd, :$Auth);
    }
    multi method new (Str $Display, *%extra) {
        my uint32 $screen;
        my $xcb = xcb_connect($Display, $screen);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }
        my $extmap;
        my $error_bases;
        my $event_bases;
        my $xge_bases;
	($extmap, $error_bases, $event_bases, $xge_bases) = ext_init($xcb);
        self.bless(:$xcb, :$extmap, :$error_bases, :$event_bases, :$xge_bases);
    }
    multi method new (Str $Display, X11::AuthInfo :$Auth!) {
        my uint32 $screen;
        my $xcb =
            xcb_connect_to_display_with_auth_info($Display, $Auth.xcb, $screen);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }
        my $extmap;
        my $error_bases;
        my $event_bases;
        my $xge_bases;
	($extmap, $error_bases, $event_bases, $xge_bases) = ext_init($xcb);
        self.bless(:$xcb, :$extmap, :$error_bases, :$event_bases, $xge_bases);
    }
    multi method new (*%extra) {
        self.new(Str, |%extra);
    }


    my sub bagify_mask(Int $event_mask is copy, Int $base is copy) {
        my $res = BagHash.new;
	while $event_mask {
            $res{$base}++ if $event_mask +& 1;
            $base++;
            $event_mask +>= 1;
        }
        $res;
    }
    my sub maskify_set(Set $b, Int $base is copy) {
        my Int $res = 0;
	for $b.keys {
            next if ($_ - $base) >= 32;
            $res +|= 1 +< ($_ - $base);
        }
        $res;
    }

    #| Start delivery of specific events to this connection, if they
    #| are not already being sent.
    #|
    #| Each X11 resource, which may belong to another client, can keep its
    #| own event mask for any X11 connection which asks for events from it.
    #| Event masks for each X11 protocol extension are kept seperately by
    #| each resource and must be set separately.  X11 resources do not,
    #| however, count how many times the same connection has asked for a
    #| particular event to be delivered.
    #|
    #| This method should be called to set event masks while thread-safely
    #| incrementing counters which keep track of how many things are
    #| interested in each category of event.  The first argument is the
    #| resource (window, or perhaps something else) which will produce events,
    #| and the second is an (extension-specific) mask of what type of events
    #| this resource should deliver to this connection.  The third argument
    #| is a type-object supplied by the extension to which the event-mask
    #| belongs, which contains instructions on how to set the mask.
    #| Usually this is available as "::EventSelector" inside the namespace of
    #| the extension.
    #|
    #| The return value will either be Bool::False, in which case there was
    #| no need to request these events because they are already being delivered,
    #| or an XCB::Cookie which could be awaited on to ensure the mask has been
    #| set before doing anything else... perhaps after flushing.
    proto method follow ($from, Int $event-mask, Selector $sel) {*}
#    multi method follow (Window $from, Int $event-mask, Selector $sel) {
#        callwith($from.wid.value, $event-mask, $event-mask, $sel)
#    }
#    multi method follow (Resource $from, Int $event-mask, Selector $sel) {
#        callwith($from.value, $event-mask, $event-mask, $sel)
#    }
    multi method follow (Int $from, Int $event-mask, Selector $sel) {
        my $emtmp = $event-mask;
        my $new = bagify_mask($event-mask, $sel.opcode +< 32);
        my $need = False;

        # We can probably share this lock
        $!event_lock.protect: {
            my $already := %.evmasks{$from};
            $already //= BagHash.new; # XXX delete this someday, fixed upstream
            if $new.Set (-) $already.Set {
               $already = ($already (+) $new).BagHash; # XXX (+) produce BagHash?
	       $need = maskify_set($already.Set, $sel.opcode);
               $need = $sel.setrq($from, $need).send($.xcb);
            }
        }
        $need;
    }
    #| Same as .follow, but stops delivery of the specified events, unless
    #| something else has also asked for them.
    proto method unfollow ($from, Int $event-mask, Selector $sel) {*}
#    multi method unfollow (Window $from, Int $event-mask, Selector $sel) {
#        callwith($from.wid.value, $event-mask, $event-mask, $sel)
#    }
#    multi method unfollow (Resource $from, Int $event-mask, Selector $sel) {
#        callwith($from.value, $event-mask, $event-mask, $sel)
#    }
    multi method unfollow (Int $from, Int $event-mask, Selector $sel) {
        my $emtmp = $event-mask;
        my $new = bagify_mask($event-mask, $sel.opcode +< 32);
        my $need = False;

        # We can probably share this lock
        $!event_lock.protect: {
            my $already := %.evmasks{$from};
            if $already.defined {
                if $already.Set (-) ($already (-) $new).Set {
                    $already = ($already (-) $new).BagHash;
	            $need = maskify_set($already.Set, $sel.opcode);
                    $need = $sel.setrq($from, $need).send($.xcb);
                }
            }
            # TODO: clean up empty masks
        }
        $need;
    }
    # TODO: facility for removing entries on resource destruction
}

our class Resource is export {
    has Connection $.from;
    has $.value;

    method new(:$from, |c) {
        $from.res_ask.send(True);
        my $value = +$from.res_chute.receive; # + to throw.
        self.bless(:$from, |c, :$value);
    }
    submethod DESTROY {
        $!from.res_scrap.send($!value);
    }
}

our class X::X11::NoSuchAtomPair is Exception {
    has $.connection;
    has $.message;
}

#| An AtomPair is the name and value of an X11 ATOM, which can also
#| remember which connection it belongs to.  It supports a .key
#| method as well to make it function a bit like a Perl 6 Pair (currently
#| it is not an Associative, but could become one in the future.)
#| Creating (or more usually, finding) an AtomPair can be done both
#| syncronously and asyncronously.
our class AtomPair is export {
    has $.name;
    has $.value;
    has $!from;

    submethod BUILD (:$!name, :$!value, :$!from) { }

    # An XCB::Cookie and X11::Connection
    my class HLCookie is Cookie {
        has Cookie $.xcb_cookie;
        has Connection $.connection;
    }

    #| Alias to .name for compatibility with Perl 6 Pair objects
    method key { $!name }

    #| The X11::Connection associated with this AtomPair,
    #| or, for unowned static Atoms, X11::XCB::AtomEnum::Atom
    method from {
        given $!from {
            when Connection { $!from }
            when HLCookie { $!from.connection }
            when AtomEnum::Atom { AtomEnum::Atom }
            when X::X11::NoSuchAtomPair { $!from.connection }
            when Failure { $!from.exception.connection }
            default { die "Should not get here " }
        }
    }

    #| The X11::XCB::xcb_connection_t associated with this AtomPair,
    #| or, for unowned static Atoms, X11::XCB::AtomEnum::Atom
    method xcb {
        given $!from {
            when Connection { $!from.xcb }
            when HLCookie { $!from.connection.xcb }
            when AtomEnum::Atom { AtomEnum::Atom }
            when X::X11::NoSuchAtomPair { $!from.connection.xcb }
            when Failure { $!from.exception.connection.xcb }
            default { die "Should not get here " }
        }
    }

    #| Syncronously finishes searching for the Atom.  If the
    #| a matching Atom is not found, throws an exception.
    #| Otherwise returns the name/key of the Atom.
    method Str {
        self.valid(:sync);
        given $!from {
            when X|Failure { .throw }
            default { $!name }
        }
    }

    #| Syncronously finishes searching for the Atom.  If the
    #| a matching Atom is not found, throws an exception.
    #| Otherwise returns the integer value of the Atom.
    method Numeric {
        self.valid(:sync);
        given $!from {
            when X|Failure { .throw }
            default { $!value }
        }
    }

    #| Returns True if the AtomPair definitely exists, or
    #| False if result is still pending or no such AtomPair exists.
    #| If :sync is provided, will wait for result.
    method valid (:$sync) {
        if $!from ~~ HLCookie and $sync {
            my $res = await $!from.xcb_cookie;
            if $res ~~ Channel {
                $res .= receive;
                if $res ~~ GetAtomNameReply {
                    with $!name {
                        if $res.name eq $!name {
                            $!from = $!from.connection;
                        }
                        else {
                            $res =
                                "Atom #$!value is named {$res.name} not $!name";
                        }
                    }
                    else {
                        $!from = $!from.connection;
                        $!name = $res.name
                    }
                }
                elsif $res ~~ InternAtomReply {
                    with $!value {
                        if +$res.atom == $!value {
                            $!from = $!from.connection;
                        }
                        else {
                            $res =
                                "Atom named \"$!name\" is #{+$res.atom} not #$!value";
                        }
                    }
                    elsif +$res.atom == 0 {
                        $res = "No dynamic atom named \"$!name\"";
                    }
                    else {
                        $!from = $!from.connection;
                        $!value = $res.atom
                    }
                }
            }
            if $res ~~ Failure {
                $!from = X::X11::NoSuchAtomPair.new(
                    :connection($!from.connection),
                    :message("Failure of type {$res.exception.^name} retrieving Atom\n" ~
                             "Original error message: {$res.exception.message}\n"));
            }
            if $res ~~ X {
                $!from = X::X11::NoSuchAtomPair.new(
                    :connection($!from.connection),
                    :message("Error of type {$res.^name} retrieving Atom\n" ~
                             "Original error message: {$res.message}\n"));
            }
            if $res ~~ Str {
                $!from = Failure.new(X::X11::NoSuchAtomPair.new(
                    :connection($!from.connection),
                    :message($res)),
                );
            }
        }
        (so $!from ~~ Connection) or (so $!from ~~ AtomEnum::Atom);
    }

    #| Find an existing atom.  If a Connection is provided as a positional,
    #| the AtomPair will remember this connection, even if the atom requested
    #| is in the static atom list.  A :name and/or a :value should be
    #| provided in either case.  Names or values not in the static Atom list
    #| will be looked for in the Connection's installed list of Atoms.
    #|
    #| See .intern if you need to create a new atom.
    proto method new ($c?, $a?, :$name, :$value) { * }

    multi method new (AtomEnum::Atom $a) {
        self.bless(:from(AtomEnum::Atom), :name($a.Str), :value($a.value));
    }
    multi method new (Connection $c, AtomEnum::Atom $a) {
        self.bless(:from($c), :name($a.Str), :value($a.value));
    }
    multi method new (Connection $c,
                      Str(Any:D) :$name!, Int(Any:D) :$value!, :$sync) {
        if AtomEnum::Atom.enums{$name} -> $v {
            # Static atom, but user wants a connection associated
            $v == $value
                ?? self.bless(:from($c), :$name, :$value)
                !! self.bless(:from(
                    Failure.new(X::X11::NoSuchAtomPair.new(
                        :connection($c),
                        :message("No static atom #$value named \"$name\""),
                    ))), :$name, :$value);
        }
        else {
            # Have to ask the server
            my $garq = GetAtomNameRequest.new(:atom($value));
            my $garp = $garq.send($c);
            my $res = self.bless(
                :from(HLCookie.new(:xcb_cookie($garp), :connection($c))),
                :$name, :$value,
            );
            $res.valid(:sync) if $sync;
            $res;
        }
    }
    multi method new (Connection $c, Str(Any:D) :$name!, :$sync) {
        if AtomEnum::Atom.enums{$name} -> $value {
            # Static atom, but user wants a connection associated
            self.bless(:from($c), :$name, :$value);
        }
        else {
            # Have to ask the server
            my $iarq = InternAtomRequest.new(:$name, :only_if_exists);
            my $iarp = $iarq.send($c);
            my $res = self.bless(
                :from(HLCookie.new(:xcb_cookie($iarp), :connection($c))),
                :$name, :value(Nil)
            );
            $res.valid(:sync) if $sync;
            $res;
        }
    }
    multi method new (Connection $c, Int(Any:D) :$value!, :$sync) {
        if AtomEnum::Atom.enums.pairs.first({$_.value == $value}) -> $p {
            # Static atom, but user wants a connection associated
            self.bless(:from($c), :name($p.key), :$value);
        }
        else {
            # Have to ask the server
            my $garq = GetAtomNameRequest.new(:atom($value));
            my $garp = $garq.send($c);
            my $res = self.bless(
                :from(HLCookie.new(:xcb_cookie($garp), :connection($c))),
                :name(Nil), :$value,
            );
            $res.valid(:sync) if $sync;
            $res;
        }
    }
    multi method new (Str(Any:D) :$name!, Int(Any:D) :$value!) {
        my $res;
        with AtomEnum::Atom.enums{$name} -> $v {
            if $v == $value {
                # Static atom, but user wants a connection associated
                $res = self.bless(:from(AtomEnum::Atom),
                                  :$name, :$value
                );
            }
        }
        $res //=
            self.bless(:from(Failure.new(X::X11::NoSuchAtomPair.new(
                :connection(AtomEnum::Atom),
                :message("No static atom #$value named \"$name\"")))),
            :$name, :$value);
        $res;
    }
    multi method new (Str(Any:D) :$name!) {
        # Ownerless static atom, lookup by name
        with AtomEnum::Atom.enums{$name} -> $value {
            self.bless(:from(AtomEnum::Atom), :$name, :$value);
        }
        else {
            self.bless(:from(Failure.new(X::X11::NoSuchAtomPair.new(
                :connection(AtomEnum::Atom),
                :message("No static atom named \"$name\"")))),
            :$name, :value(Nil));
        }
    }
    multi method new (Int(Any:D) :$value!) {
        # Ownerless static atom, lookup by value
        with AtomEnum::Atom.enums.pairs.first({$_.value == $value}) -> $p {
            self.bless(:from(AtomEnum::Atom),
                       :name($p.key), :$value);
        }
        else {
            self.bless(:from(Failure.new(X::X11::NoSuchAtomPair.new(
                :connection(AtomEnum::Atom),
                :message("No static atom #$value")))),
            :name(Nil), :$value);
        }
    }
    multi method new {
        die "{::?CLASS.^name}.new usage:\n" ~ self.^find_method("new").WHY;
    }

    #| The alternative constructor .intern may be used to install a previously
    #| nonexistent atom into a Connnection.  The Connection positional is
    #| mandatory as is a :name.  A value may not be chosen, as the Connection
    #| gets to choose that.  Note that atoms are installed permanantly for the
    #| rest of the process runtime of an X11 server, so they should be used
    #| sparingly, like unto SYSV IPC IDs.  If the atom already exists, the
    #| object is still created using the existing value.  If :sync, then the
    #| call waits for the creation to complete before returning.
    method intern (Connection $c, :$name, :$sync) {
        if AtomEnum::Atom.enums{$name} -> $value {
            # Static atom, but user wants a connection associated
            self.bless(:from($c), :$name, :$value);
        }
        else {
            # Have to ask the server
            my $iarq = InternAtomRequest.new(:$name, :!only_if_exists);
            my $iarp = $iarq.send($c);
            my $res = self.bless(
                :from(HLCookie.new(:xcb_cookie($iarp), :connection($c))),
                :$name, :value(Nil)
            );
            $res.valid(:sync) if $sync;
            $res;
        }
    }

}

our class Font is export {
    has Resource $.fid;
    has $.encoding;

    my sub encoding_from_XLFD ($fstr) {
        my @components = $fstr.split('-');
        # TODO need to handle aliases or do we always get XLFD here?
        return Nil unless +@components == 15;

        given @components[*-2,*-1].join('-') {
            when "iso8859-1" { "iso-8859-1" }
            default { $_ }
        }
    }

    has $.xcbfont handles<min_bounds max_bounds min_char_or_byte2
                          max_char_or_byte2 default_char draw_direction
                          min_byte1 max_byte1 all_chars_exist font_ascent
                          font_descent properties char_infos>;

    #| Open the first font matching the string in :first
    #| (using X11 matching/wildcard rules) and query it.
    #| This is currenty very slow.  If you do not need the information
    #| derived from a QueryFontReply, use .Open instead.
    method new(Connection $c, :$first!) {
        my $lf = ListFontsRequest.new(:max_names(1), :pattern($first));
        my $fl = await($lf.send($c)).list;
        fail("No Matching Fonts") unless $fl.elems;
        my $name = $fl[0].names[0];
        my $fid = Resource.new(:from($c));
        my $of = OpenFontRequest.new(:fid($fid.value), :$name);
        my $encoding = encoding_from_XLFD($name);
        $of.send($c);
        my $qf = QueryFontRequest.new(:font($fid.value));
        my $p = $qf.send($c);
        $c.flush; # Seems to need some encouragement
        my $xcbfont = await($p).receive;
        fail("Problem Opening Font") unless $xcbfont ~~ QueryFontReply;
        self.bless(:$fid, :$xcbfont, :$encoding);
    }

    #| Open the first font matching the string in :first
    #| (using X11 matching/wildcard rules).  Does not query the
    #| font, so many informational methods will not work until
    #| Query is run.
    method Open(Connection $c, :$first!) {
        my $lf = ListFontsRequest.new(:max_names(1), :pattern($first));
        my $fl = await($lf.send($c)).list;
        fail("No Matching Fonts") unless $fl.elems;
        my $name = $fl[0].names[0];
        my $fid = Resource.new(:from($c));
        my $of = OpenFontRequest.new(:fid($fid.value), :$name);
        my $encoding = encoding_from_XLFD($name);
        $of.send($c);
        self.bless(:$fid, :$encoding);
    }

    #| Get information about a Font from the server.  This currently
    #| takes a long time.  If called with :!sync (the default), the
    #| request is sent and a Cookie is returned while the information
    #| is made into an object in another thread.  That Cookie can then
    #| later be passed back into this method (as a value to :sync)
    #| to wait until the font information is populated, if not aready.
    #|
    #| Otherwise, if called with a true value for :sync, the Font
    #| creation will collate all the information before returning.
    multi method Query(Cookie :$sync) {
        $!xcbfont = await($sync).receive;
    }
    multi method Query(:$sync = False) {
        my $qf = QueryFontRequest.new(:font($.fid.value));
        my $p = $qf.send($.fid.from);
        if ($sync) {
            $.fid.from.flush;
            $!xcbfont = await($p).receive;
        }
        else {
            $p;
        }
    }

    submethod DESTROY {
        my $cr = CloseFontRequest.new(:font($!fid.value));
        $cr.send($!fid.from);
    }

}

our class Window is export {
    has Resource $.wid;
    method xcb { $.wid.value };

    import WindowClassEnum :enums;
    import CWEnum :enums;

    method new(Connection $c, :$map = True, :$depth = 24,
               :$x = 100, :$y = 100, :$width = 250, :$height = 250,
               :$border_width = 10,
               :$class = InputOutput,
               :$parent = $c.roots[0].root,
               :$visual = $c.roots[0].root_visual,
               *%ValueList (
                   :$BackPixmap,
                   :$BackPixel,
                   :$BorderPixmap,
                   :$CWBorderPixel,
                   :$BitGravity,
                   :$WinGravity,
                   :$BackingStore,
                   :$BackingPlanes,
                   :$BackingPixel,
                   :$OverrideRedirect,
                   :$SaveUnder,
                   :$EventMask,
                   :$DontPropagate,
                   :$Colormap,
                   :$Cursor,
                   *%ignored
               ),
        ) {
        my $wid = Resource.new(:from($c));

        my Any %value_list{CW} = CWBitGravity, 1,
                                 CWBackPixel, 0x00ffffff,
                                 CWBorderPixel, 0, 
                                 CWEventMask, 4325376,
                                 CWColormap, 0x20;

        if +%ValueList{CW.enums.keys} {
            %value_list = |%value_list, |(%ValueList.kv.map:
                -> $k, $v {
                    if CW.enums{"CW$k"}:exists {
                        |(CW(CW.enums{"CW$k"}), $v)
                    }
                }
            )
        }

        my $parentid = $parent ~~ Window ?? $parent.wid.value !! $parent;
        my $cwrq = CreateWindowRequest.new(
            :wid($wid.value),
            :$depth, :$x, :$y, :$width, :$height, :$border_width,
            :$class, :parent($parentid), :$visual, :%value_list
        );
        # Mildly cheezy here -- we create the window via the API to follow
        # its events, which is just as happy to send a CreateWindowRequest as
        # a ChangeWindowAttributesRequest.
        my class CreateWindowSelector does Selector[0] {
            method getrq ($) { Nil };
            method replymask ($) { Nil };
            method setrq ($,$) { $cwrq }
        }
        $c.follow($wid.value, %value_list{CWEventMask}, CreateWindowSelector);
        $c.flush;
        # TODO monitor errors
        if $map {
            my $mwrq = MapWindowRequest.new(:window($wid.value));
            $mwrq.send($c);
            $c.flush;
        }
        # Sanity check
        my $qtrq = QueryTreeRequest.new(:window($wid.value));
        my $cookie = $qtrq.send($c);
        $c.flush;
        my $qtrp = await($cookie).receive;
	fail("Error creating window") unless {
           ($qtrp ~~ QueryTreeReply) and $qtrp.parent == $parent;
        }
        self.bless(:$wid);
    }

    # Generic handling of simple requests with no response
    method tail-isvoid ($req, :$sync) {
        if ($sync) {
            my $fail = $req.demand($.wid.from);
            return $fail ~~ X::X11::NoReply ?? True !! $fail;
        }
        else {
            $req.send($.wid.from);
        }
    }

    method Map (:$sync = False) {
        my $mwrq = MapWindowRequest.new(:window($.wid.value));
        self.tail-isvoid($mwrq, :$sync);
    }

    method Unmap (:$sync = False) {
        my $uwrq = UnmapWindowRequest.new(:window($.wid.value));
        self.tail-isvoid($uwrq, :$sync);
    }

    submethod DESTROY {
        my $dw = DestroyWindowRequest.new(:window($!wid.value));
        $dw.send($!wid.from);
        # TODO: clean event masks from connection
    }

}

our class GC is export {
    has Resource $.cid;
    has $.drawable;

    import GCparamEnum :enums;

    method new (Connection $c, :$drawable!,
                *%ValueList (
                    :$Function,
                    :$PlaneMask,
                    :$Foreground,
                    :$Background,
                    :$LineWidth,
                    :$LineStyle,
                    :$CapStyle,
                    :$JoinStyle,
                    :$FillStyle,
                    :$FillRule,
                    :$Tile,
                    :$Stipple,
                    :$TileStippleOriginX,
                    :$TileStippleOriginY,
                    :$Font,
                    :$SubwindowMode,
                    :$GraphicsExposures,
                    :$ClipOriginX,
                    :$ClipOriginY,
                    :$ClipMask,
                    :$DashOffset,
                    :$DashList,
                    :$ArcMode,
                    *%ignored,
               ),
               ) {
        my $cid = Resource.new(:from($c));
        my $drawableid = 
           $drawable ~~ Window ?? $drawable.wid.value !! $drawable; 

        my Any %value_list{GCparam} =
            GCForeground, $c.Setup.roots[0].black_pixel,
            GCBackground, $c.Setup.roots[0].white_pixel;

        if +%ValueList{GCparam.enums.keys} {
            %value_list = |%value_list, |(%ValueList.kv.map:
                -> $k, $v {
                    if GCparam.enums{"GC$k"}:exists {
                        |(GCparam(GCparam.enums{"GC$k"}), $v)
                    }
                }
            )
        }

        my $cgrq = CreateGCRequest.new(
            :cid($cid.value), :drawable($drawableid), :%value_list
        );
        $cgrq.send($c);
        $c.flush;
        self.bless(:$cid, :$drawable);
    }

    submethod DESTROY {
        my $dgc = FreeGCRequest.new(:gc($!cid.value));
        $dgc.send($!cid.from);
    }

}

#| X11 server timestamps.  These are 32-bit milliseconds values.
#| As such, fancy wrapping rules apply.  There is also a special
#| value CurrentTime(0) which needs to be handled specially.
our class TimeStamp is Int is repr('P6opaque') is export(:internal) {
    use nqp;

    method new($val) { nqp::box_i($val, TimeStamp) }

    #| Subtract $before from $after, according to X11 timestamp wrapping rules.
    #| The result will be positive if the X11 server will consider $after to
    #| have occurred after $before, but the result is not correct for times
    #| separated by more than about 24.8 days.  A zero reult means the server
    #| will consider the two times to be equal.
    #|
    #| Note: neither argument should be 0 (CurrentTime).  That value should
    #| be checked for and handled appropriately before using this, else results
    #| may not be of any value.
    multi method infix:<-> (TimeStamp $after: TimeStamp $before)
        is export(:internal) {
        if $before < $after {
            if $after - $before <= 0x80000000 {
                # Within a half wrap (called a "month" on server)
                $after - $before;
            }
            else {
                # Outside a half wrap, therefore in the past
                -(($before +| 0x100000000) - $after);
            }
        }
        elsif $before > $after {
            if $before - $after <= 0x80000000 {
                # Within a half wrap (called a "month" on server)
                -($before - $after);
            }
            else {
                ($after +| 0x100000000) - $before;
            }
        }
        else {
            0
        }
    }

    # TODO add/subtract Int with Failure if wrap exceeded

}

our class Selection is export {

    use X11::XCB :internal;
    use X11::XCB::XProto :internal;

    has Window $.owner;
    has AtomPair $.id;
    has AtomPair @.oktargets;
    has Int $.begin;
    has Real $.end = Inf;
    has %.requests;
    has Promise $.init is rw;
    has Lock $.lock = Lock.new;
    has $.content;

    role Content { }

    #| Make :$window the owner of the selection :$id, and retain state
    #| such that :$content can be delivered to requesters of that selection.
    #| This object will pretty much handle all the finagling details of
    #| maintaining an X11 selection, just so long as all relevent events
    #| are later supplied to it with the .event method.
    proto method new (:$owner!, :$id!, :$content!) {*}

    multi method new (Window:D :$owner!, AtomPair:D :$id!,
                      Event:D :$event!, :$content!) {
        self.new(:$owner, :$id, :time($event.time), :$content);
    }
    multi method new (Window:D :$owner!, AtomPair:D :$id!,
                      Int:D :$time!, :$content!) {
        my $res = self.bless(:$owner, :$id, :begin(TimeStamp.new($time)),
                             :$content);
        $res.init = start {
            my $c = $owner.wid.from;
            my $wxcb = $owner.wid.value;
            my AtomPair @oktargets;

            my $ssorp = SetSelectionOwnerRequest.new(
                :owner($wxcb), :selection($id.value), :$time
            ).send($c);
            my $gsorp = GetSelectionOwnerRequest.new(
                :selection($id.value)
            ).send($c);
            my $x = False;
            $ssorp = await $ssorp;
            # Under normal operation we expect to receive a X::X11::NoReply
            # when waiting on the Cookie, as this is a void request.  Another
            # possibility is a single error.
            CATCH {
                when X::X11::NoReply { $x = $_; $_.resume }
                default { .message.note }
            };
            # Grab our atoms.  This will also make the above request flush
            # back out sooner.
            @oktargets = AtomPair.new($c, :name<MULTIPLE>),
                         AtomPair.new($c, :name<TARGETS>),
                         AtomPair.new($c, :name<TIMESTAMP>);

            # Provide some default behaviors for core Perl 6 types
            given $content {
                when Content { }
                when Str {
                    @oktargets.append: AtomPair.new($c, :name<UTF8_STRING>)
                }
            }
            .valid(:sync) for @oktargets;
            $res.oktargets = @oktargets;
            if      $x
                and (($gsorp .= result) ~~ Channel)
                and (($gsorp .= receive) ~~ GetSelectionOwnerReply)
                and $gsorp.owner == $wxcb {
                True
            }
            else {
                # TODO: emit some error noise maybe
                False
            }            
        }
        $res
    }

    #| Generate a key for %requests, used to guarantee sequence of processing
    #| requests which only differ in property per ICCCM conventions.
    my sub rqid (SelectionRequestEvent $event) {
        given $event {
            buf32.new(.requestor, .selection, .target, .time).decode("latin-1")
        }
    }

    #| Refuse a SelectionRequest.  Can be called without an instance, but in this
    #| case, a :connection must be supplied on which to send the refusal.
    #| Returns a Cookie which may be waited on to ensure that the refusal has
    #| been sent... perhaps after flushing.
    method refuse (SelectionRequestEvent $req,
                   :$connection = $.owner.wid.from
                  ) {
        my $sne = SelectionNotifyEvent.new(
            :requestor($req.requestor), :target($req.target), :property(None),
            :selection($req.selection), :time($req.time)
        );
        my $serp = SendEventRequest.new(
            :destination($req.requestor), :event_mask(0),
            :propagate(0), :event($sne)
        ).send($connection);
    }

    #| Notify a selection requestor that we have honored the request
    method receipt (SelectionRequestEvent $req,
                    :$connection = $.owner.wid.from
                   ) {
        my $sne = SelectionNotifyEvent.new(
            :requestor($req.requestor), :target($req.target),
            :property($req.property), :selection($req.selection),
            :time($req.time)
        );
        my $serp = SendEventRequest.new(
            :destination($req.requestor), :event_mask(0),
            :propagate(0), :event($sne)
        ).send($connection);
    }

    #| Handle a SelectionRequest.
    multi method request (Selection:D: SelectionRequestEvent $req) {
        $!init.then(
            -> $ok is copy {
                $ok = $ok.status == PromiseStatus::Kept ?? $ok.result !! False;
                my &todo;
                # TODO check timestamp
                if $ok {
                    &todo = {

                        my $c = self.owner.wid.from;
                        my $multi = False;
                        my $did = 0;
                        my @mod_targets;

                        # Legacy client support per ICCCM
                        my $property = $req.property || $req.selection;

                        my $target = @.oktargets.first(*.value == $req.target);
                        $target //= AtomPair.new($c, :value($req.target));

                        if $target.valid(:sync) {
                            my $targets;
                            my $multiatom; # So we don't have to look it back up.
                            if $target.key eq "MULTIPLE" {
                                $multi = True;
                                # TODO: convenience function for GetProperty including long properties
                                # note, after this TODO, $targets may end up being bound to a Seq 
                                # for long requests, and it should be a cached Seq
                                my $gprp = GetPropertyRequest.new(
                                    :window($req.requestor),
                                    :property($req.property),
                                    :type(AtomEnum::Atom),
                                    :long-offset(0),
                                    :long-length(1024), # We'll cap this for now
                                    :delete(+False)
                                ).send($c);
                                my $x = False;
                                $gprp = (await $gprp).receive;
                                CATCH {
                                    default { 
                                        $x = $_; .message.note; $_.resume;
                                    }
                                };
                                if (not $x) and ($gprp ~~ GetPropertyReply) {
                                    $targets = $gprp.value.values;
                                }
                            }
                            else {
                                $targets := [ $req.property, $target.value ];
                            }
                            for $targets -> $prop is copy, $targval? {

                                @mod_targets.append: $prop;

                                # Unspecced MULTIPLE corner case. Best guess.
                                $prop ||= $req.selection;

                                with $targval {
                                    @mod_targets.append: $targval;

                                    my $target = @.oktargets.first(*.value == $targval);
                                    $target //= :NONE(0);

                                    if $target.key eq "TARGETS" {
                                        my Int @oktargets = @.oktargets.map(*.value);
                                        if @oktargets.first(Int:U) === Int {
                                            warn "Could not find all atoms for TARGETS target";
                                        }
                                        # TODO: check response (but after the .follow)
                                        ChangePropertyRequest.new(
                                            :mode(PropModeEnum::Replace),
                                            :window($req.requestor),
                                            :property($prop),
                                            :type(AtomEnum::ATOM),
                                            :data(buf32.new(@oktargets.grep(Int:D)))
                                        ).send($c);
                                        $did++;
                                    }
                                    elsif $target.key eq "TIMESTAMP" {
                                        # TODO: check response (but after the .follow)
                                        ChangePropertyRequest.new(
                                            :mode(PropModeEnum::Replace),
                                            :window($req.requestor),
                                            :property($prop),
                                            :type($target.value),
                                            :data(buf32.new(+self.begin))
                                        ).send($c);
                                        $did++;
                                    }
                                    else {
                                        CATCH { $_.note }
                                        given $.content {
                                            when Content {
                                                my $type, my $data;
                                                ($type, $data) =
                                                    $.content.convert($req, $target);

                                                ChangePropertyRequest.new(
                                                    :mode(PropModeEnum::Replace),
                                                    :window($req.requestor),
                                                    :property($prop),
                                                    :type($target.value),
                                                    :$data
                                                ).send($c);

                                                @mod_targets[*-1] = $type;
                                                $did++;
                                            }
                                            when Str {
                                                my $oktarg = @.oktargets.first(*.value == $targval);
                                                if $oktarg and $oktarg.key eq "UTF8_STRING" {
                                                    ChangePropertyRequest.new(
                                                        :mode(PropModeEnum::Replace),
                                                        :window($req.requestor),
                                                        :property($prop),
                                                        :type($oktarg.value),
                                                        :data($.content.encode("utf8"))
                                                    ).send($c);
                                                    $did++;
                                                }
                                                else {
                                                    #TODO other string types
                                                    warn "NYI conversion of Str to {AtomPair.new($c,:value($targval), :sync).key}";
                                                    @mod_targets[*-1] = None;
                                                }
                                            }
                                            default {
                                                warn "NYI target type {AtomPair.new($c,:value($targval), :sync).key}";
                                                @mod_targets[*-1] = None;
                                            }
                                        }
                                    }
                                }
                                if $did == 1 {
                                    $c.follow(
                                        $req.requestor, EventMaskEnum::PropertyChange,
                                        X11::XCB::XProto::EventSelector
                                    );
                                    # TODO: check response from ChangeProperty
                                }
                            }
                        }
                        else {
                            warn "Could not find atom for target type";
                        }
                        if $did and $multi {
                            ChangePropertyRequest.new(
                                :mode(PropModeEnum::Replace),
                                :window($req.requestor),
                                :property($target),
                                :type(AtomEnum::Atom),
                                :data(buf32.new(@mod_targets))
                            ).send($c);
                            # TODO: check response
                        }
                        my $serp = 
                            $did ?? self.receipt($req) !! self.refuse($req);
                        my $x = False;
                        $serp = await $serp;
                        # Under normal operation we expect to receive a X::X11::NoReply
                        # when waiting on the Cookie, as this is a void request.  Another
                        # possibility is a single error.
                        CATCH {
                            when X::X11::NoReply { $x = $_; $_.resume }
                            default { $x.message.note }
                        };
                    }
                }
                else {
                    &todo = {
                        my $serp = self.refuse($req);
                        my $x = False;
                        $serp = await $serp;
                        # Under normal operation we expect to receive a X::X11::NoReply
                        # when waiting on the Cookie, as this is a void request.  Another
                        # possibility is a single error.
                        CATCH {
                            when X::X11::NoReply { $x = $_; $_.resume }
                            default { $x.message.note }
                        };
                        False
                    }
                }
                my $rqid = rqid($req);
                $!lock.protect: {
                    if %!requests{$rqid}:exists {
                        %!requests{$rqid} .= then(&todo);
                    }
                    else {
                        %!requests{$rqid} = Promise.start(&todo);
                    }
                }
            }
        );
    }

    multi method end (Int $time is copy) {
        $!lock.protect: { $!end = $time; }
    }
    multi method end (Event $event) {
        $!lock.protect: { $!end = $event.time; }
    }
}

