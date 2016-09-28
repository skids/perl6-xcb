unit module X11;

use X11::XCB;
use X11::XCB::XProto;

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

our class Connection is export {
    has xcb_connection_t $.xcb;
    has $.Setup;

    has uint32 $!screen = 0;
    has xcb_query_extension_reply_t $!ext; # should never be freed
    has Channel $.cookies = Channel.new();
    has Channel $.watch = Channel.new();
    has Channel $.destroying = Channel.new();
    #| Extension base value followed by Error code to type maps from
    #| said extension, pre-sorted by descending base value.
    has $.error_bases is rw = [ 0, $X11::XCB::XProto::errorcodes ];
    has $.event_bases is rw = [ 0, $X11::XCB::XProto::eventcodes ];
    #| Lock for preventing simultaneous access to error/event bases
    has Lock $.error_bases_lock = Lock.new();
    has Lock $.event_bases_lock = Lock.new();

    method flush { xcb_flush($!xcb) }

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
    # the parent gracefuly.
    my sub resource_server(Channel $scrap, Channel $ask,
                           xcb_connection_t $c is raw --> Channel) {
        my @scrapheap;
        my Channel $res = Channel.new;

        start { CATCH { $_.say }; react {
            whenever $scrap {
                @scrapheap.push: $_;
                LAST { @scrapheap = () }
                QUIT { @scrapheap = () }
            }
            whenever $ask {
                my $r = +@scrapheap ?? @scrapheap.shift !! xcb_generate_id($c);
                $res.send($r);
                LAST { $res.close }
                QUIT { $res.fail }
            }	    
        }}
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
                    if $v ~~ Promise::Vow;
                $v.fail($msg) if $v ~~ Channel;
            };
            # XXX race
            $cookies.close;
	    $r.fail($msg) if $r;
            while $watcher.poll -> $v {
                $v.break(Failure.new($msg))
                    if $v ~~ Promise::Vow;
                $v.fail($msg) if $v ~~ Channel;
            };
            # XXX race
            $watcher.close;
            if $watch.defined {
                $watch.fail($msg);
            }
            $destroying.close;
            xcb_disconnect($xcb);
        }

        my sub error_to_exception($err) {
            return Nil unless $err.defined;
            my $left = Inf;
            #TODO extension subclasses
            my $res = RequestError.subclass(nativecast(Pointer,$err),
                                            :$error_bases, :$error_bases_lock,
                                            :$left, :free);
            use X::Protocol::X11;
            X::Protocol::X11.new(:status($res.error_code),
                                 :sequence($res.sequence),
                                 :major_opcode($res.major_opcode),
                                 :minor_opcode($res.minor_opcode),
                                 :bad_value($res.bad_value));
        }

        start { CATCH { $_.say }; react {
            # Handles destroying when nobody is waiting on a Cookie.
            whenever $destroying { destroy }

            # Both these channels are functionally equivalent, but
            # $cookies is used internally for requests.  $watch is
            # supposed to be used by the API user to change where events
            # go if they want to (possibly) not wait for cookies to
            # finish before that happens.
            whenever $watch|$cookies -> $_ is copy {

                my Channel $responses;
                my Promise $sent;
                my $follow-after;

                loop {

                    my sub running_poll {
                        $watch.poll orelse $cookies.poll
                    }

                    # Check for shutdown
                    if $destroying.poll {
                        destroy($sent);
                        last
                    }
                    xcb_select_r($fd, 100000);
                    # Were we shutdown while waiting?
                    if $destroying.poll {
                        destroy($sent);
                        last
                    }

                    # Values that alter where unowned events are sent
                    if $_ ~~ Int {
                        # Turn off events (discard them)
                        $watcher.close if ($watcher.defined);
                        $watcher = Nil;
                        last;
                    }
                    if $_ ~~ Channel {
                        # Change the channel on which unowned events are queued
                        $_.send($watcher) if ($watcher.defined); # For chaining
                        $watcher = $_;
                        $_ = running_poll;
			next;
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
                        # XXX workaround for negative uint8s.  Needs RT.
                        elsif ($gev.code +& 0x7f) > 1 {
                            # An event.  If we have a watcher, use it.
                            if $watcher.defined {

                                my class eventstub does X11::XCB::Event[0] { 
                                    method cstruct { X11::XCB::Event::cstruct };
                                }

                                my $left = Inf;
                                # TODO thread, serialize, extension event classes
                                my $res = eventstub.subclass(
                                    nativecast(Pointer,$ev), :$left, :free,
                                    :$event_bases, :$event_bases_lock);
                                $watcher.send($res)
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

                    unless $_.defined {
                        last unless $watcher.defined;
                        $_ = running_poll;
                        next;
                    }

                    my $status = xcb_poll_for_reply($xcb, .promise.sequence, $r, $e);
                    if $status {
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
                                $sent.then({
                                    my $ev = error_to_exception($e2);
                                    $c.fail($ev === Any ?? $e2 !! $ev)
                                });
                            }
                            else {
                                my $p = $_;
                                my $e2 = $e;
                                start {
                                    my $ev = error_to_exception($e2);
                                    $p.break($ev === Any ?? $e2 !! $ev)
                                }
                            }
                            last unless $watcher.defined;
                            $_ = running_poll;
                            next;
                        }
                        elsif $r == $null {
                            # Definitively no more results.
                            if ($sent.defined) {
                                my $ev = %errors{.promise.sequence +& 0xffff}:delete;
                                my $c = $responses;
                                $sent.then({
                                    $ev = error_to_exception($ev);
                                    $ev === Any ?? $c.close !! $c.fail($ev);
                                })
                            }
                            else {
                                my $ev = %errors{.promise.sequence +& 0xffff}:delete;
                                my $p = $_;
                                start {
                                    $ev = error_to_exception($ev);
                                    $p.break($ev === Any ?? "No Response" !! $ev);
                                }
                            };
                            last unless $watcher.defined;
                            $_ = running_poll;
                            next;
                        }
                        else {
                            if ($sent.defined) {
                                my $c = $responses;
                                my $p = $_;
                                my $r2 = $r;
                                $sent .= then(
                                    {
                                        $c.send: $p.promise.reply_type.new(
                                            $r2, :left(Int), :free
                                        );
                                    }
                                );
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
                    # received so we'll never get blocking in the middle
                    # of a string of replies.  So, we can just use state to
                    # figure out what happened.
                    elsif ($sent.defined) {
                        my $c = $responses;
                        $sent.then({$c.close});
                        last unless $watcher.defined;
                        $_ = running_poll;
                        next;
                    }
                }
            }
        }}
    }
    has $.waiter = wait($!cookies, $!watch, $!destroying, $!xcb,
                        $!error_bases, $!error_bases_lock,
                        $!event_bases, $!event_bases_lock);

    method DESTROY {
        $.res_ask.close;
        $.res_scrap.close;
        $.destroying.send(True);
    }

    multi method new (Int $fd!, X11::AuthInfo :$Auth) {
        my $xcb = xcb_connect_to_fd($fd, $Auth);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }

        self.bless(:$xcb);
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
        self.bless(:$xcb, :$screen);
    }
    multi method new (Str $Display, X11::AuthInfo :$Auth!) {
        my uint32 $screen;
        my $xcb =
            xcb_connect_to_display_with_auth_info($Display, $Auth.xcb, $screen);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }
        self.bless(:$xcb, :$screen);
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
    has $.xcbfont handles<min_bounds max_bounds min_char_or_byte2
                          max_char_or_byte2 default_char draw_direction
                          min_byte1 max_byte1 all_chars_exist font_ascent
                          font_descent properties char_infos>;

    #| Open the first font matching the string in :first
    #| (using X11 matching/wildcard rules).
    method new(Connection $c, :$first!) {
        my $lf = ListFontsRequest.new(:max_names(1), :pattern($first));
        my $fl = await($lf.send($c)).list;
        fail("No Matching Fonts") unless $fl.elems;
        my $name = $fl[0].names[0];
        my $fid = Resource.new(:from($c));
        my $of = OpenFontRequest.new(:fid($fid.value), :$name);
        $of.send($c);
        my $qf = QueryFontRequest.new(:font($fid.value));
        my $p = $qf.send($c);
        $c.flush; # Seems to need some encouragement
        my $xcbfont = await($p).receive;
        fail("Problem Opening Font") unless $xcbfont ~~ QueryFontReply;
        self.bless(:$fid, :$xcbfont);
    }

    submethod DESTROY {
        my $cr = CloseFontRequest.new(:font($!fid.value));
        $cr.send($!fid.from);
    }

}

our class Window is export {
    has Resource $.wid;

    import WindowClassEnum :enums;

    method new(Connection $c, :$map = True, :$depth = 24,
               :$x = 100, :$y = 100, :$width = 250, :$height = 250,
               :$border_width = 10,
               :$class = InputOutput,
               :$parent = $c.roots[0].root,
               :$visual = $c.roots[0].root_visual) {
        my $wid = Resource.new(:from($c));
        my %value_list = "16" => 1, "2" => 0x00ffffff, "8" => 0, "2048" => 4325376, "8192",0x20;
        my $cwrq = CreateWindowRequest.new(
            :wid($wid.value),
            :$depth, :$x, :$y, :$width, :$height, :$border_width,
            :$class, :$parent, :$visual, :%value_list
        );
        $cwrq.send($c);
        $c.flush;
        # TODO monitor errors
        if $map {
            my $mwrq = MapWindowRequest(:window($wid.value));
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

    submethod DESTROY {
        my $dw = DestroyWindowRequest.new(:window($!wid.value));
        $dw.send($!wid.from);
    }

}


#my class xcb_intern_atom_reply_t is repr("CStruct") {
#    has uint8 $.response_type;
#    has uint8 $.pad0_0;
#    has uint16 $.sequence;
#    has uint32 $.length;
#    has uint32 $.atom;
#}
#use NativeCall;
#my sub xcb_intern_atom (xcb_connection_t $c,
#                 uint8 $o,
#                 uint16 $l,
#                 Str $n --> uint32) is native("xcb") { }

#my sub xcb_intern_atom_reply (xcb_connection_t $c,
#                       uint32 $cookie,
#                       Pointer $e is rw) is native("xcb") { }

#method ia {
#  use NativeCall :types;
#  my Pointer $p;
#  my Pointer $e;
#
#  my $x = InternAtomRequest.new(:name(""),:!only_if_exists);
#  my $ret = $x.send(self);
##  my $seq1 = $x.xcb_send_request($!xcb, $x.bufs);
##  xcb_flush($!xcb);
##  $seq1.say;
##  my $ret = Cookie.new(:sequence($seq1), :reply_type($x.reply));
##my $v = $ret.vow;
##:$v.say;
##  $.cookies.send($v);
#  xcb_flush($!xcb);
#  $ret;
#}

#method gs {
#  xcb_get_setup($!xcb);
#}

#method wfe {
#start {
#$!xcb.perl.say;
#xcb_wait_for_event($!xcb).perl.say;
#"got".say;
#}
#}

#}




#my $c = Connection.new();
#sleep 1;

#{
#use NativeCall;
#my $pr = $c.ia;
#$pr.say;
#my $p = await $pr;
#$c.ia;
#$p.list.say;
#}

#42.say;

#$c = Nil;
#sleep 2;

