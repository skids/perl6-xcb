use v6;
use lib <blib/lib lib>;

use Test;

plan 12;

use NativeCall;
use X11::XCB;
use X11::XCB::XProto;

throws-like 'String.new(:name("OHAI" x 64)).bufs',
  Exception, message => /:i size\s+exceeded/,
  "String > 255 chars dies if bufferized";

given String.new(:name<feefiefoofum>) {
  my $what = "String with normal contents";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);
  is @bufs, (12,|"feefiefoofum".encode.values), "$what bufferizes correctly";
  my $c = nativecast(.cstruct,Buf.new(@bufs));
  is-deeply $c, .cstruct.new(:name_len(12)), "$what roundtrip to cstruct";
  my $left = 13;
  my $s = String.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.name, $_.name, "$what roundtrip to perl instance";
  $left = 12;
  throws-like 'String.new(nativecast(Pointer,$c), :$left, :!free)',
      Exception, message => /:i short\s+packet/, "$what dies on short buffer";
}

given String.new(:name("")) {
  my $what = "String with no contents";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);
  is @bufs, (0,), "$what bufferizes correctly";
  my $c = nativecast(.cstruct,Buf.new(@bufs));
  is-deeply $c, .cstruct.new(:name_len(0)), "$what roundtrip to cstruct";
  my $left = 1;
  my $s = String.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.name, $_.name, "$what roundtrip to perl instance";
}



given GetWindowAttributesRequest.new(:window(42)) {
  my $what = "Request with simple fixed field";
  is .bufs».values, (3,0,2,0,0x2a,0,0,0), "$what bufferizes correctly";
}

given InternAtomRequest.new(:only_if_exists, :name<foo>) {
  my $what = "Request with one charfield aligned -1";
  is .bufs».values, (0x10,1,3,0,3,0,0,0,0x66,0x6f,0x6f,0), "$what bufferizes correctly";
}

given InternAtomRequest.new(:only_if_exists, :name<foo+1>) {
  my $what = "Request with one charfield aligned +1";
  is .bufs».values, (16,1,4,0,5,0,0,0,102,111,111,43,49,0,0,0), "$what bufferizes correctly";
}

given InternAtomRequest.new(:only_if_exists, :name<foo0>) {
  my $what = "Request with one charfield aligned";
  is .bufs».values, (0x10,1,3,0,4,0,0,0,0x66,0x6f,0x6f,48), "$what bufferizes correctly";
}

given InternAtomRequest.new(:only_if_exists, :name<feefiefoo>) {
  my $what = "Request with one charfield aligned +1 (larger)";
  is .bufs».values, (16,1,5,0,9,0,0,0,102,101,101,102,105,101,102,111,111,0,0,0), "$what bufferizes correctly";
}

given InternAtomRequest.new(:only_if_exists, :name("")) {
  my $what = "Request with one charfield null";
  is .bufs».values, (0x10,1,2,0,0,0,0,0), "$what bufferizes correctly";
}

given RotatePropertiesRequest.new(:window(0) :atoms(840,841,842,843,844,845,846) :delta(3)) {
  my $what = "Request with one charfield null";
  is .bufs».values, (0x10,1,2,0,0,0,0,0), "$what bufferizes correctly";

}


class QER
    does X11::XCB::Request[99,
                 xcb_extension_t,
                 False] {

    my $.reply = Array;

    my $.cstruct = class :: is repr("CStruct") {

        has uint8 $.major_opcode is rw;
        has uint8 $.pad0_0;
        has uint16 $.length is rw;

        method Hash {
            {

            }
        }
        method nativeize($p6) {
            $!major_opcode = 99;
        }
    };



    has $.sequence is rw;


    method child_bufs {
        my @bufs;

        |@bufs;
    }

}

{ use X11;
my $c = X11::Connection.new();
my $q = ListExtensionsRequest.new;
my $co = $q.send($c);
$c.flush;
my $rep = await $co;
$rep.list.perl.say;
}
