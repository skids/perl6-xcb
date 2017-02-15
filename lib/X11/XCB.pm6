unit module X11::XCB;

# This is just the base API of XCB, so we can leave the
# trickier (event/socket handling, etc) stuff to libxcb.
# We don't offer a one-to-one mapping to all XCB API,
# partly because NativeCall cannot handle passing structs on
# the stack and partly because it is not -Ofun to be working
# with leak liabilities everywhere.

# Generally you use the X11 module instead of this, which uses
# all this stuff internally.

use NativeCall;

sub padbuf(Int $bytes) is export(:internal) {
    given $bytes {
        when 0 { |() };
        default { Blob.new(0 xx $bytes) };
    }
}

# Convenience function, not part of the XCB api
# This is enough to get us working on Linux...
# solving this portably is a matter for other ecosystem.
sub xcb_select_r(uint32 $fd, Int $us) is export {
    my class timeval is repr("CStruct") {
        has long    $.tv_sec = 0;
        has long    $.tv_usec = 0;
    }
    my sub select(int32 $nfds, CArray[uint8] $rfds,
                  CArray[uint8] $wfds, CArray[uint8] $efds,
                  timeval $timeout) is native { };

    # We're guessing here that select may be implemented to
    # read up to nfds in chunks dependent on the CPU.
    # Well assume 128 for future-proofing.  Maybe the ecosystem
    # will gain a module that does the macro work and we
    # can use that instead.
    my $fd_setsize = (($fd + 1 + 127) div 128) * 128 / 8;
    my $rfds = CArray[uint8].new(0 xx $fd_setsize);
    $rfds[$fd div 8] = 1 +< ($fd % 8);
    my $to = timeval.new(:tv_usec($us));
    select($fd + 1, $rfds, CArray[uint8], CArray[uint8], $to);
}

# Likewise, until the ecosystem provides this roll our own.
our class xcb_iovec is repr("CStruct") is export(:internal) {
    has size_t  $.iov_base is rw; # XXX should be Pointer not size_t
                                  # (but "is rw" and Pointer NYI)
                                  # (so no running this on AS/200)
    has size_t  $.iov_length is rw;
}

our sub xcb_free (Pointer $mem)
    is native is symbol("free") is export(:internal) {*}

# Stub structures for things that would normally be imported
# from xproto.h, just to keep API look and feel consistent.
# Note we don't support actually manipulating them, they should
# be fed into a corresponding deserialization constructor
# for the Perl 6 binding for that object.

class xcb_setup_t is repr('CPointer') is export { }
class xcb_generic_event_t is repr('CPointer') is export { }
class xcb_generic_error_t is repr('CPointer') is export { }
class xcb_query_extension_reply_t is repr('CStruct') is export {
    has uint8 $.response_type is rw;
    has uint8 $.pad0_0;
    has uint16 $.sequence is rw;
    has uint32 $.length is rw;
    has uint8 $.present is rw;
    has uint8 $.major_opcode is rw;
    has uint8 $.first_event is rw;
    has uint8 $.first_error is rw;
}

# Now the actual XCB core.

class xcb_connection_t is repr('CPointer') is export {

    sub xcb_disconnect (xcb_connection_t $c)
        is native("xcb") is export { * }

    submethod DESTROY {
        xcb_disconnect(self);
    }
}

# We bypass he global synbols so we don't need the .so files
# (and they were proving impossible to get working anyway)
# class xcb_extension_t is repr('CPointer') is export(:internal :DEFAULT) { }
class xcb_extension_t is repr('CStruct') is export(:internal :DEFAULT) {
    has size_t $.name is rw; # Why str/Str does not work for us here is WAT
    has uint32 $.num is rw;
}

class xcb_auth_info_t is repr('CStruct') is export {

    #| number of bytes of $!name when utf8-encoded, auto-adjusted
    #! unless explicitly set after $!name
    has int32 $.namelen;

    #| string containing the authentication protocol name, such as
    #| "MIT-MAGIC-COOKIE-1" or "XDM-AUTHORIZATION-1".  When this
    #| is set, $!namelen will be automatically adjusted appropriately
    has str $!name;

    #| number of bytes of $!data when utf8-encoded, auto-adjusted
    #| unless explicitly set after $!data
    has int32 $.datalen;

    #| interpreted in a protocol-specific manner, depending on $!name
    has str $!data;

    submethod BUILD (str :$data, str :$name,
                     :$datalen = $data.defined ??
                          0 !! $data.encode("utf8").bytes,
                     :$namelen = $name.defined ??
                          0 !! $name.encode("utf8").bytes) {
        $!data = $data;
        $!name = $name;
        $!datalen = $datalen;
        $!namelen = $namelen;
    }

    multi method name is rw {
        Proxy.new(:FETCH(-> $ { $!name })
                  :STORE(-> $, $v {
                             $!namelen = $v.encode("utf8").bytes;
                             $!name = $v
                         }
                         )
                  );
    }

    multi method data is rw {
        Proxy.new(:FETCH(-> $ { $!name })
                  :STORE(-> $, $v {
                             $!datalen = $v.encode("utf8").bytes;
                             $!data = $v
                         }
                         )
                  );
    }

    # Should not need these, I don't think.  Avoids immutability error.
    multi method namelen is rw { $!namelen }
    multi method datalen is rw { $!datalen }
};

sub xcb_connect (str $display, int32 $screen is rw --> xcb_connection_t)
    is native("xcb") is export { * }

sub xcb_connect_to_display_with_auth_info (str $display, xcb_auth_info_t $auth,
                                           int32 $screen is rw
                                           --> xcb_connection_t)
    is native("xcb") is export { * }

sub xcb_connect_to_fd (int32 $fd, xcb_auth_info_t $auth_info
                       --> xcb_connection_t)
    is native("xcb") is export { * }

sub xcb_parse_display (str $name,
                       Pointer[Str] $host is rw,
                       int32 $display is rw,
                       int32 $screen --> int32 )
    is native("xcb") is export { * }

#| A version of xcb_parse_display which safely frees the hostname malloc,
#| which returns Str:U on failure or the hostname in a GCd Perl6 Str:D.
sub xcb_parse_display_Str (str $name, int32 $display is rw,
                                int32 $screen is rw --> Str) {
    my Pointer[Str] $h = Pointer[Str].new;
    LEAVE {
        sub free(Pointer $) is native { * };
        free $h;
    }
    xcb_parse_display($name, $h, $display, $screen) ?? $h.deref !! Str;
}

sub xcb_get_setup (xcb_connection_t $c --> xcb_setup_t)
    is native("xcb") is export { * }

sub xcb_get_file_descriptor (xcb_connection_t $c --> int32) 
    is native("xcb") is export { * }

sub xcb_get_maximum_request_length (xcb_connection_t $c --> uint32)
    is native("xcb") is export { * }

sub xcb_wait_for_event (xcb_connection_t $c --> xcb_generic_event_t)
    is native("xcb") is export { * }

sub xcb_poll_for_event (xcb_connection_t $c --> xcb_generic_event_t)
    is native("xcb") is export { * }

sub xcb_wait_for_reply (xcb_connection_t $c, uint32 $request,
                        xcb_generic_error_t $e is rw --> Pointer)
    is native("xcb") is export { * }

sub xcb_poll_for_reply (xcb_connection_t $c, uint32 $request,
                        Pointer $reply is rw,
                        xcb_generic_error_t $e is rw --> int32)
    is native("xcb") is export { * }

sub xcb_connection_has_error (xcb_connection_t $c --> int32)
    is native("xcb") is export { * }

sub xcb_flush (xcb_connection_t $c --> int32)
    is native("xcb") is export { * }

sub xcb_get_extension_data (xcb_connection_t $c, xcb_extension_t $ext
                            --> xcb_query_extension_reply_t)
    is native("xcb") is export { * }

sub xcb_prefetch_extension_data (xcb_connection_t $c, xcb_extension_t $ext)
    is native("xcb") is export { * }

sub xcb_prefetch_maximum_request_length (xcb_connection_t $c)
    is native("xcb") is export { * }

sub xcb_generate_id (xcb_connection_t $c --> uint32)
    is native("xcb") is export { * }

enum ConnError <<
    :ERROR(1) EXT_NOTSUPPORTED MEM_INSUFFICIENT REQ_LEN_EXCEED
    PARSE_ERR INVALID_SCREEN FDPASSING_FAILED
>>;

class xcb_protocol_request_t is repr("CStruct") is export(:internal) {
    has size_t $.count is rw;
    # XXX should be: has xcb_extension_t $.ext is rw;
    has size_t $.ext is rw = 0;
    has uint8 $.opcode is rw;
    has uint8 $.isvoid is rw;
};

enum xcb_send_request_flags <<
    :XCB_REQUEST_CHECKED(1) :XCB_REQUEST_RAW(2)
    :XCB_REQUEST_DISCARD_REPLY(4) :XCB_REQUEST_REPLY_FDS(8)
>>;

sub xcb_send_request(xcb_connection_t $c, int32 $flags, xcb_iovec $vector,
                     xcb_protocol_request_t $request --> int32)
    is native("xcb") is export { * }

use X::Protocol;
class X::Protocol::XCB is X::Protocol is export {
    method protocol { "X11 C Binding API" }
    multi method new (Int :$status) {
        nextwith(:status(ConnError($status)));
    }
    multi method new (ConnError :$status) {
        nextsame;
    }
    method codes {
        Hash[Str,ConnError].new(
            ERROR, "socket, pipe or other stream error.",
            EXT_NOTSUPPORTED, "extension not supported",
            MEM_INSUFFICIENT, "out of memory",
            REQ_LEN_EXCEED, "request length exceeded",
            PARSE_ERR, "error parsing display string",
            INVALID_SCREEN, "no such screen on server",
            FDPASSING_FAILED, "could not hand off file descriptor"
        )
    }
    method severity {
        given self.status {
            when 0 { "success" };
            default { "error" }
        }
    }
}

class Cookie is Promise is export {
    has uint64 $.sequence;
    has $.reply_type;
}

our role Error[$error_code] is export(:internal) {

    method cstruct {...}

    #| Create a new X11 protocol error Perl6 object.
    #| A first parameter, if provided, is a Pointer to buffer data.
    #| If provided, :left designates the length of data (in bytes) available
    #| in the buffer, and safety checks will prevent reading off the end.
    #| :left may be undefined to trust the data's self-proclaimed length.
    #| Safety checks will be promulgated to substructures in either case.
    #| If :left refers to an lvalue, it will be modified to contain the
    #| amount of excess space in the buffer.
    #| If :free is set (the default) the buffer is assumed
    #| to point to a natively allocated buffer and the
    #| the Pointer will be freed.
    #| If no Pointer is provided, attributes may be initialized normally.
    multi method new (Pointer $p, Int :$left! is rw, Bool :$free = True) {
        my $cs = nativecast(self.cstruct, $p);
        $left -= nativesizeof(self.cstruct);
        fail("Short packet.") unless $left >= 0;
        my $res = self.bless(|$cs.Hash);
        xcb_free $p if $free;
        $res;
    }
    multi method new (Pointer $p, Int :$left!, Bool :$free = True) {
        my Int $l = $left;
        self.new($p, :left($l), :$free);
    }
    multi method new (Pointer $p, Bool :$free = True) {
        my $left = 262140; # XXX actual max size
        self.new($p, :$left, :$free);
    }
    multi method new (|c) {
        nextsame;
    }

    method Buf {
        # XXX This is not technically safe.  We want GC memory,
        # and aliases into it, but GC memory can move anytime.
        my $len = nativesizeof($.cstruct);
        my $res = Buf.new(0 xx $len);
        my $c := nativecast($.cstruct, $res);
        $c.nativize(self);
        $res;
    }
    method bufs {
        self.Buf, |self.child_bufs;
    }

    method error_code($c) {
        $error_code; # XXX need to add extension base
    }

    #| Find the appropriate subclass based on error codes.
    #| This is more a method of the role group than of any
    #| parameterization.  Takes a (positional) pointer to a raw protocol
    #| packet containing an error, a :error_bases array containing a list
    #| of alternating error base values and errorcode maps, and an
    #| associated :error_lock to serialize access.  An lvalue :length to
    #| prevent buffer overruns must be supplied (but can be Inf).
    #| This will be altered based on the encountered structure.
    #| Finally is :!free is not passed the raw protocol packet will
    #| be freed.
    method subclass (Pointer $p, :$error_bases!, :$error_lock!,
                     :$left! is rw, :$free = True) {

        my class errorstub is repr("CStruct") {
            has uint8 $!reponse_type;
            has uint8 $.error_code;
        }
        die "Packet too short" unless $left >= nativesizeof(errorstub);
        # XXX workaround negative uint8.  Needs RT
        my $code = nativecast(errorstub, $p).error_code +& 0xff;
        my $cl;
        $error_lock.protect: {
            for |$error_bases -> $k, $v {
                next if $k > $code;
                if $v{$code - $k}:exists {
                    $cl = $v{$code - $k};
                    last;
                }
            }
        };
        die "Unknown error code, or extension IDs somehow failed to cache"
            if $cl === Any;
        $cl.new($p, :$left, :$free);
    }
}

our role Event[$event_code] is export(:internal) {

    method cstruct {...}

    # XXX Review: this was originally exported but rakudo changed something
    my class Event::cstruct is repr("CStruct") {
        has uint8 $.code;
        has uint8 $.detail;
        has uint16 $.sequence;
    }

    #| Create a new X11 protocol event Perl6 object.
    #| A first parameter, if provided, is a Pointer to buffer data.
    #| If provided, :left designates the length of data (in bytes) available
    #| in the buffer, and safety checks will prevent reading off the end.
    #| :left may be undefined to trust the data's self-proclaimed length.
    #| Safety checks will be promulgated to substructures in either case.
    #| If :left refers to an lvalue, it will be modified to contain the
    #| amount of excess space in the buffer.
    #| If :free is set (the default) the buffer is assumed
    #| to point to a natively allocated buffer and the
    #| the Pointer will be freed.
    #| If no Pointer is provided, attributes may be initialized normally.
    multi method new (Pointer $p, Int :$left! is rw, Bool :$free = True) {
        my $cs = nativecast(self.cstruct, $p);
        $left -= nativesizeof(self.cstruct);
        fail("Short packet.") unless $left >= 0;
        my $res = self.bless(|$cs.Hash);
        xcb_free $p if $free;
        $res;
    }
    multi method new (Pointer $p, Int :$left!, Bool :$free = True) {
        my Int $l = $left;
        self.new($p, :left($l), :$free);
    }
    multi method new (Pointer $p, Bool :$free = True) {
        my $left = 262140; # XXX actual max size
        self.new($p, :$left, :$free);
    }
    multi method new (|c) {
        nextsame;
    }

    method Buf {
        # XXX This is not technically safe.  We want GC memory,
        # and aliases into it, but GC memory can move anytime.
        my $len = nativesizeof($.cstruct);
        my $res = Buf.new(0 xx $len);
        my $c := nativecast($.cstruct, $res);
        $c.nativize(self);
        $res;
    }
    method bufs {
        self.Buf, |self.child_bufs;
    }

    method event_code($c) {
        $event_code; # XXX need to add extension base
    }

    #| Find the appropriate subclass based on event codes.
    #| This is more a method of the role group than of any
    #| parameterization.  Takes a (positional) pointer to a raw protocol
    #| packet containing an event, a :event_bases array containing a list
    #| of alternating event base values and eventcode maps, and an
    #| associated :event_lock to serialize access.  An lvalue :length to
    #| prevent buffer overruns must be supplied (but can be Inf).
    #| This will be altered based on the encountered structure.
    #| Finally is :!free is not passed the raw protocol packet will
    #| be freed.
    method subclass (Pointer $p, :$event_bases!, :$event_lock!,
                     :$left! is rw, :$free = True) {

        die "Packet too short" unless $left >= nativesizeof(Event::cstruct);
        my $code = nativecast(Event::cstruct, $p).code +& 0x7f;
        my $cl;
        $event_lock.protect: {
            for |$event_bases -> $k, $v {
                next if $k > $code;
                if $v{$code - $k}:exists {
                    $cl = $v{$code - $k};
                    last;
                }
            }
        };
        die "Unknown event code, or extension IDs somehow failed to cache"
            if $cl === Any;
        $cl.new($p, :$left, :$free);
    }
}

our role Struct is export(:internal) {

    method cstruct {...}

    method child_structs(Pointer $p, $pstruct, Int :$left! is rw) {...}

    #| Create a new X11 protocol substructure Perl6 object.
    #| A first parameter, if provided, is a Pointer to buffer data.
    #| If provided, :left designates the length of data (in bytes) available
    #| in the buffer, and safety checks will prevent reading off the end.
    #| :left may be undefined to trust the data's self-proclaimed length.
    #| Safety checks will be promulgated to substructures in either case.
    #| If :left refers to an lvalue, it will be modified to contain the
    #| amount of excess space in the buffer.
    #| If :free is set (the default) the buffer is assumed
    #| to point to a natively allocated buffer and the
    #| the Pointer will be freed.
    #| If no Pointer is provided, attributes may be initialized normally.
    multi method new (Pointer $p, Int :$left! is rw, Bool :$free = True) {
        my $cs = nativecast(Pointer[$.cstruct], $p).deref;
        $left -= nativesizeof($.cstruct);
	fail("Short packet.") unless $left >= 0;
        my %childinits;
        for (|$cs.Hash.kv, |self.child_structs(Pointer[uint8].new(
                                   nativecast(Pointer[uint8], $p)
                                   + nativesizeof($.cstruct)
                               ), $cs, :$left)) -> $k, \v {
            use nqp;
            # XXX graceful way to decont without NQP or assuming listiness?
            %childinits{$k} := nqp::decont(v);
        }
        my $res = ::?CLASS.bless(|%childinits);
        xcb_free $p if $free;
        $res;
    }
    multi method new (Pointer $p, Int :$left!, Bool :$free = True) {
        my Int $l = $left;
        self.new($p, :left($l), :$free);
    }
    multi method new (Pointer $p, Bool :$free = True) {
        my $left = 262140; # XXX actual max size
        self.new($p, :$left, :$free);
    }
    multi method new (|c) {
        nextsame;
    }

    method Buf {
        # XXX This is not technically safe.  We want GC memory,
        # and aliases into it, but GC memory can move anytime.
        my $len = nativesizeof($.cstruct);
        my $res = Buf.new(0 xx $len);
        my $c := nativecast($.cstruct, $res);
        $c.nativize(self);
        $res;
    }
    method bufs {
        self.Buf, |self.child_bufs;
    }

}

our role Reply [$opcode] is export(:internal) {

    my $.opcode = $opcode;

    method cstruct {...}

    method child_bufs { |() }

    method Buf {
        # XXX This is not technically safe.  We want GC memory,
        # and aliases into it, but GC memory can move anytime.
        my $len = nativesizeof($.cstruct);
        my $res = Buf.new(0 xx $len);
        my $c := nativecast($.cstruct, $res);
        $c.nativize(self);
        $res;
    }

    method bufs {
        my @bufs = self.Buf, self.child_bufs;
        # XXX safe to use GC memory like this?
        my $c := nativecast($.cstruct, @bufs[0]);
        my $length = [+] @bufs».bytes;
        die "BUG: absurd length of buffers"
            if $length > 0x3ffffffff;
        $length -= 32;
        my $pad = -$length +& 3;
        $c.length = ($length + 3) +> 2;
        (|@bufs, padbuf($pad));
    }

    #| Create a new X11 protocol reply Perl6 object.
    #| A first parameter, if provided, is a Pointer to buffer data.
    #| If provided, :left designates the length of data (in bytes) available
    #| in the buffer, and safety checks will prevent reading off the end.
    #| :left may be undefined to trust the data's self-proclaimed length.
    #| Safety checks will be promulgated to substructures in either case.
    #| If :left refers to an lvalue, it will be modified to contain the
    #| amount of excess space in the buffer.
    #| If :free is set (the default) the buffer is assumed
    #| to point to a natively allocated buffer and the
    #| the Pointer will be freed.
    #| If no Pointer is provided, attributes may be initialized normally.
    multi method new (Pointer $p, Int :$left! is rw, Bool :$free = True) {
        my $cs = nativecast(Pointer[$.cstruct], $p).deref;
        without $left {
            # We have to trust that the buffer is long enough to read
            # the length field and that the length field is accurate
            $left = 32 + $cs.length * 4;
        }
        $left -= nativesizeof($.cstruct);
	fail("Short packet.") unless $left >= 0;
        my %childinits;
        for (|$cs.Hash.kv, |self.child_structs(Pointer[uint8].new(
                                   nativecast(Pointer[uint8], $p)
                                   + nativesizeof($.cstruct)
                               ), $cs, :$left)) -> $k, \v {
            use nqp;
            # XXX graceful way to decont without NQP or assuming listiness?
            %childinits{$k} := nqp::decont(v);
        }
        my $res = ::?CLASS.bless(|%childinits);
        xcb_free $p if $free;
        $res;
    }
    multi method new (Pointer $p, Int :$left!, Bool :$free = True) {
        my Int $l = $left;
        self.new($p, :left($l), :$free);
    }
    multi method new (Pointer $p, Bool :$free = True) {
        my $left = Int;
        self.new($p, :$left, :$free);
    }
    multi method new (|c) {
        nextsame;
    }

}

our role Request [$opcode, $ext, $isvoid] is export(:internal) {

    method xcb_protocol_request_t (|c) {
        if $ext.defined {
            xcb_protocol_request_t.new(:count(1), :ext(nativecast(Pointer,$ext()).Int), :$opcode, :$isvoid, |c);
        }
        else {
            xcb_protocol_request_t.new(:count(1), :$opcode, :$isvoid, |c);
        }
    }

    method cstruct {...}

    method child_bufs { |() }

    method Buf {
        # XXX This is not technically safe.  We want GC memory,
        # and aliases into it, but GC memory can move anytime.
        my $len = nativesizeof($.cstruct);
        my $res = Buf.new(0 xx $len);
        my $c := nativecast($.cstruct, $res);
        $c.nativize(self);
        $res;
    }

    method bufs {
        my @bufs = self.Buf, self.child_bufs;
        # XXX safe to use GC memory like this?
        my $c := nativecast($.cstruct, @bufs[0]);
        my $length = [+] @bufs».bytes;
        die "BUG: absurd length of buffers"
            if $length > 0x3ffff;
        my $pad = -$length +& 3;
        $c.length = ($length + 3) +> 2;
        (|@bufs, padbuf($pad));
    }

    # This method does not use self (and we rely on that
    # elsewhere.)  Maybe should make it into a sub.
    method xcb_send_request($c, @bufs) {
        my $vecs = Buf.new(0 xx +@bufs * nativesizeof(xcb_iovec));
        # XXX safe to use GC memory like this?
	my $ca = nativecast(Pointer, $vecs);

        for @bufs.kv -> $n, $b {
            my $v = nativecast(xcb_iovec, $ca);
            $v.iov_base = nativecast(Pointer, $b);
            $v.iov_length = $b.bytes;
            $ca = Pointer.new($ca + nativesizeof(xcb_iovec));
        }

        my $rq = self.xcb_protocol_request_t(:count(+@bufs));

        xcb_send_request($c, 0, nativecast(xcb_iovec, $vecs), $rq);
    }

    #| Send this request to a connection.  If an xcb_connection_t
    #| is provided, a sequence number is returned.  If an
    #| X11::Connection is provided, returns an X11::Cookie.
    #|
    #| This method will automatically figure out extension opcodes
    #| and packet lengths at the time the request packet is sent.
    #| It is safe to send the same request to different connections,
    #| as they are not modified by this method.
    # Low-level access.
    multi method send(xcb_connection_t $xcb) {
        self.xcb_send_request($xcb, self.bufs);
    }

    # A higher level Connection object.
    multi method send($c) {
        $.sequence = self.send($c.xcb);
	my $ret = Cookie.new(:$.sequence, :reply_type($.reply));
        $c.cookies.send($ret.vow);
        $ret;
    }

    #| Syncronous version of .send.  Does not return until all
    #| replies and/or errors to the request have been received.
    #| Multiple replies will be returned as a Slip.
    method demand($c) {
        my @r = await(self.send($c)).list[*];
        @r.elems == 1 ?? @r[0] !! |@r;
    }
}

# XXX POD on this causes compilation issues
#   #| Interface defining how to set/get an event-mask for events.  Should
#   #| be supplied for each event-mask by the responsible module.
our role Selector [Int $opcode] is export {
#    #| used an an extension identfier
    method opcode { $opcode };

#    #| build a request that will set $from's event-mask to $event-mask
    method setrq (uint32 $from, uint32 $event-mask --> Request) {...};

#    #| build a request that will get $from's event-mask
    method getrq (uint32 $from --> Request) {...};

#    #| extract the event-mask from the $reply to the request made by .getrq
    method mask (Reply $reply --> uint32) {...};
}


