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

our class Connection is export {
    has xcb_connection_t $.xcb;
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
    #| Lock for preventing simultaneous access to error/event bases
    has Lock $.error_bases_lock = Lock.new();
    has Lock $.event_bases_lock = Lock.new();

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

        start { CATCH { $_.say }; react {
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
                $error_bases, $error_bases_lock,
                $event_bases, $event_bases_lock) {

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
                                            :$error_bases, :$error_bases_lock,
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

            if not $bad_value.defined and (nativesizeof($res.cstruct) >= 8) {
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

        start { CATCH { $_.say };
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

                            $evsent .= then({
                                my $left = Inf;
                                my $res = eventstub.subclass(
                                    nativecast(Pointer,$ev), :$left, :free,
                                    :$event_bases, :$event_bases_lock
                                );
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
                                $p.break($ev === Any ?? "No Response" !! $ev);
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
                            $sent .= then({
                                $c.send: $p.promise.reply_type.new(
                                    $r2, :left(Int), :free
                                );
                            });
                        }
                        else {
                            # We could just build a list but a Channel allows
                            # multiple workers to respond.  Don't know if there
                            # are any actual use cases for this, though.
                            $responses = Channel.new;
                            my $c = $responses;
                            my $p = $_;
                            my $r2 = $r;
                            $sent = start {
                                $p.keep($c);
                                $c.send: $p.promise.reply_type.new(
                                    $r2, :left(Int), :free
                                );
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
                        $!error_bases, $!error_bases_lock,
                        $!event_bases, $!event_bases_lock);

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
            }
        }
        @errorcodes.push(Pair.new(0, $X11::XCB::XProto::errorcodes));
        @eventcodes.push(Pair.new(0, $X11::XCB::XProto::eventcodes));

        @errorcodes .= sort: *.key;
        @eventcodes .= sort: *.key;

        $extmap,
        [@errorcodes.reverse.map({|($_.key,$_.value)})],
        [@eventcodes.reverse.map({|($_.key,$_.value)})];
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
	($extmap, $error_bases, $event_bases) = ext_init($xcb);
        self.bless(:$xcb, :$extmap, :$error_bases, :$event_bases);
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
	($extmap, $error_bases, $event_bases) = ext_init($xcb);
        self.bless(:$xcb, :$extmap, :$error_bases, :$event_bases);
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
	($extmap, $error_bases, $event_bases) = ext_init($xcb);
        self.bless(:$xcb, :$extmap, :$error_bases, :$event_bases);
    }
    multi method new (*%extra) {
        self.new(Str, |%extra);
    }
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
        $cwrq.send($c);
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
            return $fail.message ~~ 'No Response' ?? True !! $fail;
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
    }

}

our class GC is export {
    has Resource $.cid;
    has $.drawable;

    import GCFieldEnum :enums;

    method new(Connection $c, :$drawable!) {
        my $cid = Resource.new(:from($c));
        my $drawableid = 
           $drawable ~~ Window ?? $drawable.wid.value !! $drawable; 
        my Any %value_list{GCField} =
            GCForeground, $c.Setup.roots[0].black_pixel,
            GCBackground, $c.Setup.roots[0].white_pixel;
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
