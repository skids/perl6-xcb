use v6;
use lib <blib/lib lib>;

use Test;

plan 47;

use X11;
use X11::XCB::RandR;

use NativeCall;
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


is NotifyData::cstruct.wiresize, 28, "NotifyData CUnion has correct wiresize";
my $ev = CArray[uint8].new(1,2,uint16_bytes(42), (uint32_bytes($_) for 1..4),5, ^11);
my $p = nativecast(Pointer[uint8], $ev);
my $left = 32;
my $n = RandRNotifyEvent.new($p, :$left, :!free);
ok  $n.isa(RandRNotifyEvent), "Made a RandRNotifyEvent from bytes";
ok $left == 0, "RandRNotifyEvent construction subtracted correct length";
for -1..31 {
    $left = $_;
    dies-ok -> {RandRNotifyEvent.new($p, :$left, :!free)}, "RandrNotifyEvent short/bad length $_ detected";
}
ok $n.u.isa(OutputProperty), "RandRNotifyEvent chose/built OutputProperty from subCode/u";
is ($n.sequence, $n.u.window, $n.u.output, $n.u.atom, $n.u.timestamp, $n.u.status), (42, 1..5), "RandRNotifyEvent/OutputProperty all fields have expected values";
is +OutputProperty, 2, "OutputProperty numifies to its subcode";
my $o = OutputProperty.new(:1window, :2output, :3atom, :4timestamp, :5status);
ok $o.isa(OutputProperty), "Made an OutputProperty by hand";
is ($o.window, $o.output, $o.atom, $o.timestamp, $o.status), (1..5), "Hand made OutputProperty all fields have expected values";
my $e = RandRNotifyEvent.new(:42sequence, :u($o));
ok $e.isa(RandRNotifyEvent), "Made a RandRNotifyEvent by hand";
is ($e.sequence, $e.u.window, $e.u.output, $e.u.atom, $e.u.timestamp, $e.u.status), (42, 1..5), "Hand made RandRNotifyEvent/OutputProperty all fields have expected values";
is (|(.values for $e.bufs)).flat[^21], ($ev.values)[^21], "Hand made RandRNotifyEvent/OutputProperty serializes to correct bytes";
is &NotifyData(2), OutputProperty, "NotifyData coercer with Int";
is &NotifyData(OutputProperty), OutputProperty, "NotifyData coercer idempotence";
is &NotifyData()[2].gist, Pair.new("2",OutputProperty).gist, "NotifyData list works";

#my $c = Connection.new;
#my $sir = RandRSelectInputRequest.new(:window($c.roots[0].root), :enable(1 +< +ResourceChange));
#$sir.perl.say;
#$sir.send($c);

#sleep 10;
