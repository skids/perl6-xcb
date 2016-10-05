use v6;
use lib <blib/lib lib>;

use Test;

plan 151;

use NativeCall;
use X11::XCB;
use X11::XCB::XProto;

# Get the bytes of a uint32 as stored in a CStruct on this system
my sub uint32_bytes ($v) {
    my $cl = class :: is repr("CStruct") { has uint32 $.v };
    |(nativecast(CArray[uint8], $cl.new(:$v))[^4]);
}
# Get the bytes of a uint16 as stored in a CStruct on this system
my sub uint16_bytes ($v) {
    my $cl = class :: is repr("CStruct") { has uint16 $.v };
    |(nativecast(CArray[uint8], $cl.new(:$v))[0,1]);
}
# Get a uint32 as aliased to two uint16s in a CStruct on this system
my sub uint16x2_uint32 ($v1, $v2) {
    my $c32 = class :: is repr("CStruct") { has uint32 $.v };
    my $c16 = class :: is repr("CStruct") { has uint16 $.v1;
                                            has uint16 $.v2 };
    nativecast($c32, $c16.new(:$v1, :$v2)).v;
}
# Get a uint32 as aliased to four uint8s in a CStruct on this system
my sub uint16x4_uint8 ($v1, $v2, $v3, $v4) {
    my $c32 = class :: is repr("CStruct") { has uint32 $.v };
    my $c8 = class :: is repr("CStruct") {
        has uint8 $.v1;
        has uint8 $.v2;
        has uint8 $.v3;
        has uint8 $.v4;
    }
    nativecast($c32, $c8.new(:$v1, :$v2, :$v3, :$v4)).v;
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

  is @bufs, @(1,0,uint16_bytes(0),uint32_bytes(2),|^32), "$what bufferizes correctly";

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
      :31keys___pad31, :2length, :0sequence, :1response_type), "$what roundtrip to cstruct";
  my $left = 40;
  my $s = QueryKeymapReply.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply $s.keys, $_.keys, "$what roundtrip to perl instance";
}

given ClientMessageEvent.new(:sequence(0x1234),:window(3),:type(4),:data(Buf[uint32].new(1..5))) {
  my $what = "32-bit ClientMessageEvent";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);

  is @bufs, @(33,32,|uint16_bytes(0x1234),|(uint32_bytes($_) for (3,4,1,2,3,4,5))), "$what bufferizes correctly";

  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:33response_type, :4660sequence,
    :32format, :3window, :4type, :1cmd___pad0,
    :2cmd___pad1, :3cmd___pad2, :4cmd___pad3, :5cmd___pad4,
  ), "$what roundtrip to cstruct";
  my $left = 32;
  my $s = ClientMessageEvent.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply ($s.window, $s.type, $s.data), ($_.window, $_.type, $_.data), "$what roundtrip to perl instance";
}

given ClientMessageEvent.new(:sequence(0),:window(3),:type(4),:data(Buf[uint16].new(1..10))) {
  my $what = "16-bit ClientMessageEvent";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);

  is @bufs, @(33,16,0,0,|(uint32_bytes($_) for (3,4)), |(uint16_bytes($_) for 1..10)), "$what bufferizes correctly";

  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:33response_type,
    :0sequence, :16format, :3window, :4type, 
    :cmd___pad0(uint16x2_uint32(1,2)),
    :cmd___pad1(uint16x2_uint32(3,4)),
    :cmd___pad2(uint16x2_uint32(5,6)),
    :cmd___pad3(uint16x2_uint32(7,8)),
    :cmd___pad4(uint16x2_uint32(9,10))), "$what roundtrip to cstruct";
  my $left = 32;
  my $s = ClientMessageEvent.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply ($s.window, $s.type, $s.data), ($_.window, $_.type, $_.data), "$what roundtrip to perl instance";
}

given ClientMessageEvent.new(:sequence(0),:window(3),:type(4),:data(Buf[uint8].new(1..20))) {
  my $what = "8-bit ClientMessageEvent";
  ok $_.defined, "$what create perl instance";

  my @bufs = (|$_ for .bufs);

  is @bufs, @(33,8,0,0,|(uint32_bytes($_) for (3,4)), |(1..20)), "$what bufferizes correctly";

  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);
  is-deeply $c, .cstruct.new(:33response_type,
    :0sequence, :8format, :3window, :4type, 
    :cmd___pad0(uint16x4_uint8(1,2,3,4)),
    :cmd___pad1(uint16x4_uint8(5,6,7,8)),
    :cmd___pad2(uint16x4_uint8(9,10,11,12)),
    :cmd___pad3(uint16x4_uint8(13,14,15,16)),
    :cmd___pad4(uint16x4_uint8(17,18,19,20))), "$what roundtrip to cstruct";
  my $left = 32;
  my $s = ClientMessageEvent.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply ($s.window, $s.type, $s.data), ($_.window, $_.type, $_.data), "$what roundtrip to perl instance";
}

given GetPropertyReply.new(:format(8), :sequence(0x1234),:type(1),:bytes_after(2), :value(array[uint8].new(1..16))) {
  my $what = "8-bit GetPropertyReply";
  ok $_.defined, "$what create perl instance";
  my @bufs = (|$_ for .bufs);
  is @bufs, @(1, 8, |uint16_bytes(0x1234), |(uint32_bytes($_) for (4,1,2)),|uint32_bytes(16),0 xx 12,|(1..16)), "$what bufferizes correctly";

  my $b = Buf.new(@bufs);
  my $c = nativecast(.cstruct,$b);

  is-deeply $c, .cstruct.new(
    :4660sequence, :8format, :2bytes_after, :1type, :16value_len,
    :0pad0_0, :0pad0_1, :0pad0_2, :0pad0_3, :0pad0_4, :0pad0_5,
    :0pad0_6, :0pad0_7, :0pad0_8, :0pad0_9, :0pad0_10, :0pad0_11,
    :length(Int(16 / 4)), :1response_type
    ), "$what roundtrip to cstruct";
  my $left = 48;
  my $s = GetPropertyReply.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply ($s.type, $s.bytes_after, $s.value), ($_.type, $_.bytes_after, $_.value), "$what roundtrip to perl instance";
}

given GetPropertyReply.new(:format(16), :type(1),:bytes_after(2), :value(array[uint16].new(501..516))) {
  my $what = "16-bit GetPropertyReply";
  ok $_.defined, "$what create perl instance";
  my @bufs = (|$_ for .bufs);

  is @bufs, @(1, 16, |uint16_bytes(0), |(uint32_bytes($_) for (8,1,2)),|uint32_bytes(16),0 xx 12,|(501..516)), "$what bufferizes correctly";

  my $b = Buf.new(.bufs[0].values,|(uint16_bytes($_) for 501..516));
  my $c = nativecast(.cstruct,$b);

  is-deeply $c, .cstruct.new(
    :16format, :2bytes_after, :1type, :16value_len,
    :0pad0_0, :0pad0_1, :0pad0_2, :0pad0_3, :0pad0_4, :0pad0_5,
    :0pad0_6, :0pad0_7, :0pad0_8, :0pad0_9, :0pad0_10, :0pad0_11,
    :length(Int(2 * 16 / 4)), :1response_type
    ), "$what roundtrip to cstruct";
  my $left = 64;
  my $s = GetPropertyReply.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply ($s.type, $s.bytes_after, $s.value), ($_.type, $_.bytes_after, $_.value), "$what roundtrip to perl instance";
}

given GetPropertyReply.new(:format(32), :type(1),:bytes_after(2), :value(array[uint32].new(66001..66016))) {
  my $what = "32-bit GetPropertyReply";
  ok $_.defined, "$what create perl instance";
  my @bufs = (|$_ for .bufs);

  is @bufs, @(1, 32, |uint16_bytes(0), |(uint32_bytes($_) for (16,1,2)),|uint32_bytes(16),0 xx 12,|(66001..66016)), "$what bufferizes correctly";

  my $b = Buf.new(.bufs[0].values,|(uint32_bytes($_) for 66001..66016));
  my $c = nativecast(.cstruct,$b);

  is-deeply $c, .cstruct.new(
    :32format, :2bytes_after, :1type, :16value_len,
    :0pad0_0, :0pad0_1, :0pad0_2, :0pad0_3, :0pad0_4, :0pad0_5,
    :0pad0_6, :0pad0_7, :0pad0_8, :0pad0_9, :0pad0_10, :0pad0_11,
    :length(Int(4 * 16 / 4)), :1response_type
    ), "$what roundtrip to cstruct";
  my $left = 96;
  my $s = GetPropertyReply.new(nativecast(Pointer,$c), :$left, :!free);
  is $left, 0, "$what unpacking subtracts its length correctly";
  is-deeply ($s.type, $s.bytes_after, $s.value), ($_.type, $_.bytes_after, $_.value), "$what roundtrip to perl instance";
}

#given RotatePropertiesRequest.new(:window(0) :atoms(840,841,842,843,844,845,846) :delta(3)) {
#  my $what = "Request with one charfield null";
#  is .bufs».values, (0x10,1,2,0,0,0,0,0), "$what bufferizes correctly";
#}

