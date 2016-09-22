use v6;
use lib <blib/lib lib>;

use Test;

plan 116;

use NativeCall;
use X11::XCB;
use X11::XCB::XProto;

# Get the bytes of a uint32 as stored in a CStruct on this system
my sub uint32_bytes ($v) {
    my $cl = class :: is repr("CStruct") { has uint32 $.v };
    |(nativecast(CArray[uint8], $cl.new(:$v))[^4]);
}

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

use X11::XCB::XPrint;

given Printer.new(:name<myprinter>, :description<mineminemine!>) {
  my $what = "interleaved length field with normal contents";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);
  # XXX get this confirmation value endian-adj


  is @bufs, (uint32_bytes(9),|"myprinter".encode.values,
             uint32_bytes(13),|"mineminemine!".encode.values,
             ), "$what bufferizes correctly";
  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:nameLen(9)), "$what roundtrip to cstruct";
  my $left = 4 + 9 + 4 + 13;
  my $s = Printer.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.name, $_.name, "$what roundtrip to perl instance";
  for ^(4 + 13 + 4 + 9) {
      $left = $_;
      throws-like 'Printer.new(nativecast(Pointer,$c), :$left, :!free)',
          Exception, message => /:i short\s+packet/, "$what dies on short buffer ($_)";
  }
  is-deeply .name, "myprinter", "$what .name is a Str";
  is-deeply .description, "mineminemine!", "$what .description is a Str";
}

given Printer.new(:name(""), :description<mineminemine!>) {
  my $what = "interleaved length field with first field null";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);
  # XXX get this confirmation value endian-adj


  is @bufs, (uint32_bytes(0),
             uint32_bytes(13),|"mineminemine!".encode.values,
             ), "$what bufferizes correctly";
  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:nameLen(0)), "$what roundtrip to cstruct";
  my $left = 4 + 4 + 13;
  my $s = Printer.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.name, $_.name, "$what roundtrip to perl instance";
  for ^(4 + 13 + 4) {
      $left = $_;
      throws-like 'Printer.new(nativecast(Pointer,$c), :$left, :!free)',
          Exception, message => /:i short\s+packet/, "$what dies on short buffer ($_)";
  }
  is-deeply .name, "", "$what .name is a Str";
  is-deeply .description, "mineminemine!", "$what .description is a Str";
}

given Printer.new(:name<foo>, :description("")) {
  my $what = "interleaved length field with second field null";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);
  # XXX get this confirmation value endian-adj


  is @bufs, (uint32_bytes(3), |"foo".encode.values,
             uint32_bytes(0)
             ), "$what bufferizes correctly";
  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:nameLen(3)), "$what roundtrip to cstruct";
  my $left = 4 + 3 + 4;
  my $s = Printer.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.name, $_.name, "$what roundtrip to perl instance";
  for ^(4 + 3 + 4) {
      $left = $_;
      throws-like 'Printer.new(nativecast(Pointer,$c), :$left, :!free)',
          Exception, message => /:i short\s+packet/, "$what dies on short buffer ($_)";
  }
  is-deeply .name, "foo", "$what .name is a Str";
  is-deeply .description, "", "$what .description is a Str";
}

given Printer.new(:name(""), :description("")) {
  my $what = "interleaved length field with both fields null";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);
  # XXX get this confirmation value endian-adj


  is @bufs, (uint32_bytes(0),
             uint32_bytes(0)
             ), "$what bufferizes correctly";
  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:nameLen(0)), "$what roundtrip to cstruct";
  my $left = 4 + 4;
  my $s = Printer.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.name, $_.name, "$what roundtrip to perl instance";
  for ^(4 + 4) {
      $left = $_;
      throws-like 'Printer.new(nativecast(Pointer,$c), :$left, :!free)',
          Exception, message => /:i short\s+packet/, "$what dies on short buffer ($_)";
  }
  is-deeply .description, "", "$what .description is a Str";
  is-deeply .name, "", "$what .name is a Str";
}


given QueryKeymapReply.new(:keys[^32]) {
  my $what = "fixed length array (and a reply)";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);

  is @bufs, @(uint32_bytes(0),uint32_bytes(2),|^32), "$what bufferizes correctly";

  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:0keys___pad0, :1keys___pad1, :2keys___pad2,
      :3keys___pad3, :4keys___pad4, :5keys___pad5, :6keys___pad6,
      :7keys___pad7, :8keys___pad8, :9keys___pad9, :10keys___pad10,
      :11keys___pad11, :12keys___pad12, :13keys___pad13, :14keys___pad14, 
      :15keys___pad15, :16keys___pad16, :17keys___pad17, :18keys___pad18, 
      :19keys___pad19, :20keys___pad20, :21keys___pad21, :22keys___pad22,
      :23keys___pad23, :24keys___pad24, :25keys___pad25, :26keys___pad26, 
      :27keys___pad27, :28keys___pad28, :29keys___pad29, :30keys___pad30, 
      :31keys___pad31, :2length, :0sequence), "$what roundtrip to cstruct";
  my $left = 40;
  my $s = QueryKeymapReply.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.keys, $_.keys, "$what roundtrip to perl instance";
#  for ^(4 + 4) {
#      $left = $_;
#      throws-like 'Printer.new(nativecast(Pointer,$c), :$left, :!free)',
#          Exception, message => /:i short\s+packet/, "$what dies on short buffer ($_)";
#  }
#  is-deeply .description, "", "$what .description is a Str";
#  is-deeply .name, "", "$what .name is a Str";
}



#given RotatePropertiesRequest.new(:window(0) :atoms(840,841,842,843,844,845,846) :delta(3)) {
#  my $what = "Request with one charfield null";
#  is .bufs».values, (0x10,1,2,0,0,0,0,0), "$what bufferizes correctly";
#}

#class QER
#    does X11::XCB::Request[99,
#                 xcb_extension_t,
#                 False] {
#
#    my $.reply = Array;
#
#    my $.cstruct = class :: is repr("CStruct") {
#
#        has uint8 $.major_opcode is rw;
#        has uint8 $.pad0_0;
#        has uint16 $.length is rw;
#
#        method Hash {
#            {
#
#            }
#        }
#        method nativeize($p6) {
#            $!major_opcode = 99;
#        }
#    };
#
#
#
#    has $.sequence is rw;
#
#
#    method child_bufs {
#        my @bufs;
#
#        |@bufs;
#    }
#
#}
#
#{ use X11;
#my $c = X11::Connection.new();
#my $q = ListExtensionsRequest.new;
#my $co = $q.send($c);
#$c.flush;
#my $rep = await $co;
#$rep.list.perl.say;
#}
