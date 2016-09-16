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

    has $.jiggle_seq;
    has uint32 $!screen = 0;
    has xcb_query_extension_reply_t $!ext; # should never be freed
    has Channel $.cookies = Channel.new();
    has Channel $.destroying = Channel.new();

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
    my sub wait(Channel $cookies, Channel $destroying,
                xcb_connection_t $xcb is raw,
                $jiggle is copy) {

        my $fd = xcb_get_file_descriptor($xcb);

        my sub destroy {
            while $cookies.poll -> $v {
                $v.break(Failure.new("Connection has been closed"))
            };
            $cookies.close;
            $destroying.close;
            xcb_disconnect($xcb);
        }

        start { CATCH { $_.say }; react {
            # Handles destroying when nobody is waiting on a Cookie.
            whenever $destroying { destroy }
            whenever $cookies {
                use NativeCall;

                # Use xcb handle as a sentry value.
                my $sentry = nativecast(Pointer, $xcb);
                my Channel $follow-on;
                my $follow-after;

                loop {
                    my Pointer $e = $sentry.clone;
                    my Pointer $r = $sentry.clone;
                    constant $null = Pointer.new(0);

                    xcb_flush($xcb);
                    # Should return immediately since it is for an old reply
                    xcb_wait_for_reply($xcb, $jiggle, $e);
                    xcb_flush($xcb);

                    # Use xcb handle as a sentry value.
                    $e = $sentry.clone;

                    my $status = xcb_poll_for_reply($xcb, .promise.sequence, $r, $e);
                    if $status {
                        # Definitive answer
                        if $r == $sentry or $e == $sentry {
                            die("BUG: xcb_poll_for_reply returned 1 without" ~
                                " altering both reply and error.\n" ~
                                "BUG: This should be impossible.  API changed?");
                        }
                        if $e !== $null {
                            $jiggle = .promise.sequence;
                            if ($follow-on) { $follow-on.fail($e) } # TODO encapsulate
                            else { .break($e) }; # TODO encapsulate
                            $follow-on = Nil;
                            last;
                        }
                        elsif $r == $null {
                            # Definitively no more results.
                            $jiggle = .promise.sequence;
                            if ($follow-on) { $follow-on.close }
                            else { .break("No Response") };
                            last;
                        }
                        else {
                            if ($follow-on) {
#                                $follow-after = start { 
                                    $follow-on.send: .promise.reply_type.new(
                                        $r, :left(Int), :free
                                    );
#                                }
                            }
                            else {
                                # We could just build a list but a Channel allows
                                # multiple workers to respond.  Don't know if there
                                # are any actual use cases for this, though.
                                $follow-on = Channel.new;
#                                start {
                                    my $rr =  .promise.reply_type.new(
                                        $r, :left(Int), :free
                                    );
                                    $follow-on.send: $rr;
                                    .keep($follow-on);
#                                }
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
                    elsif ($follow-on) {
                        $follow-on.close;
                        $jiggle = .promise.sequence;
                        last;
                    }
                    # Nothing yet.  Yield till there might be.
                    # But first check for shutdown
                    if $destroying.poll { destroy; last } # TODO follow-on
                    xcb_select_r($fd, 100000);
                    # Were we shutdown while destroying?
                    if $destroying.poll { destroy; last } # TODO follow-on
                }
            }
        }}
    }
    has $.waiter = wait($!cookies, $!destroying, $!xcb, $!jiggle_seq);

    method DESTROY {
        $.res_ask.close;
        $.res_scrap.close;
        $.destroying.send(True);
    }

    my sub jiggle_polling($xcb) {
        # You have to jiggle the handle to get polling working.
        use NativeCall;
        my sub xcb_get_atom_name (xcb_connection_t $c, uint32 $atom --> uint32)
            is native("xcb") { }
        my Pointer $e = nativecast(Pointer, $xcb); # sentry value;
        my $s = xcb_get_atom_name($xcb, 1);
        xcb_flush($xcb);
        xcb_wait_for_reply($xcb, $s, $e);
        say "Error priming the request/reply pump." if $e ne Pointer.new(0);
        $s;
    }

    multi method new (Int $fd!, X11::AuthInfo :$Auth) {
        my $xcb = xcb_connect_to_fd($fd, $Auth);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }
        $.jiggle_seq = jiggle_polling($xcb);

        self.bless(:$xcb, :jiggle_seq(jiggle_polling($xcb)));
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
        self.bless(:$xcb, :$screen, :jiggle_seq(jiggle_polling($xcb)));
    }
    multi method new (Str $Display, X11::AuthInfo :$Auth!) {
        my uint32 $screen;
        my $xcb =
            xcb_connect_to_display_with_auth_info($Display, $Auth.xcb, $screen);
        if xcb_connection_has_error($xcb) -> $status {
            xcb_disconnect($xcb);
            fail X::Protocol::XCB.new(:$status);
        }
        self.bless(:$xcb, :$screen, :jiggle_seq(jiggle_polling($xcb)));
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
        xcb_flush($c.xcb); # Seems to need some encouragement
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

    method new(Connection $c, :$map = True, :$depth = 24,
               :$x = 100, :$y = 100, :$width = 250, :$height = 250,
               :$border_width = 10,
               :$class = XProto::WindowClass::InputOutput,
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

