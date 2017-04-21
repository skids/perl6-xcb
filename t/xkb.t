use v6;
use lib <blib/lib lib>;

use Test;

plan 34;


use X11;
use X11::XCB::xkb;
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


is Action::cstruct.wiresize, 8, "Action CUnion has correct wiresize";
is SIAction.cstruct.wiresize, 8, "SIAction generic has correct wiresize";
my $ev = CArray[uint8].new(uint32_bytes(1),2,3,4,5,20,1,2,3,4,5,6,7);
my $p = nativecast(Pointer[uint8], $ev);
my $left = 16;
my $n = SymInterpret.new($p, :$left, :!free);
ok  $n.isa(SymInterpret), "Made a SymInterpret from bytes";
ok $left == 0, "SymInterpret construction subtracted correct length";
for -1..15 {
    $left = $_;
    dies-ok -> {SymInterpret.new($p, :$left, :!free)}, "SymInterpret short/bad length $_ detected";
}
ok $n.action.isa(SADeviceValuator), "SymInterpret chose/built SADeviceValuator from tagged union";
is ($n.sym, $n.mods, $n.match, $n.virtualMod, $n.flags), (1,2,3,4,5), "SymInterpret all fields have expected values";
is ($n.action.device,
    $n.action.val1what, $n.action.val1index, $n.action.val1value,
    $n.action.val2what, $n.action.val2index, $n.action.val2value
), (1,2,3,4,5,6,7), "SADeviceValuator all fields have expected values";
is +SADeviceValuator, 20, "SADeviceValuator numifies to its subcode";
my $o = SADeviceValuator.new(:1device, :2val1what, :3val1index, :4val1value, :5val2what, :6val2index, :7val2value);
ok $o.isa(SADeviceValuator), "Made an SADeviceValuator by hand";
is ($o.device, $o.val1what, $o.val1index, $o.val1value, $o.val2what, $o.val2index, $o.val2value), (1..7), "Hand made SADeviceValuator all fields have expected values";
my $e = SymInterpret.new(:1sym, :2mods, :3match, :4virtualMod, :5flags, :action($o));
ok $e.isa(SymInterpret), "Made a SymInterpret by hand";
is ($e.sym, $e.mods, $e.match, $e.virtualMod, $e.flags), (1,2,3,4,5), "Hand made SymInterpret all fields have expected values";
is ($e.action.device,
    $e.action.val1what, $e.action.val1index, $e.action.val1value,
    $e.action.val2what, $e.action.val2index, $e.action.val2value
), (1,2,3,4,5,6,7), "Hand made SymInterpret/SADeviceValuator all fields have expected values";
is (|(.values for $e.bufs)).flat[^16], ($ev.values)[^16], "Hand made SymInterpret/SADeviceValuator serializes to correct bytes";
is &Action(20), SADeviceValuator, "Action coercer with Int";
is &Action(SADeviceValuator), SADeviceValuator, "Action coercer idempotence";
is &Action()[0].gist, Pair.new("0",SANoAction).gist, "Action list works";

#my $c = Connection.new;
#my $sir = xkbSelectInputRequest.new(:window($c.roots[0].root), :enable(1 +< +ResourceChange));
#$sir.perl.say;
#$sir.send($c);

#sleep 10;

#my $c = Connection.new;
#my $gdi = GetDeviceInfoRequest.new(:deviceSpec<IDEnum::UseCoreKbd>);
#ok $gdi.isa(GetDeviceInfoRequest), "Made a GetDeviceInfoRequest";

#my $cookie = $gdi.send($c);
#my $res = await($cookie).receive;

#$res.perl.say
