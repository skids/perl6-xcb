use v6;
use lib <blib/lib lib>;

use Test;

plan 69;

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

is Behavior::cstruct.wiresize, 2, "Behavior CUnion has correct wiresize";
is CommonBehavior.cstruct.wiresize, 2, "CommonBehavior generic has correct wiresize";
$ev = CArray[uint8].new(0,2);
$p = nativecast(Pointer[uint8], $ev);
$left = 2;
$n = CommonBehavior.new($p, :$left, :!free);
ok  $n.isa(DefaultBehavior), "Made a DefaultBehavior from bytes (CommonBehavior.new)";
ok $left == 0, "DefaultBehavior construction subtracted correct length";
for -1..1 {
    $left = $_;
    dies-ok -> {Behavior.new($p, :$left, :!free)}, "Behavior short/bad length $_ detected";
}
$left = 2;
$n = Behavior.new($p, :$left, :!free);
ok  $n.isa(DefaultBehavior), "Made a DefaultBehavior from bytes (Behavior.new)";
ok $left == 0, "DefaultBehavior construction subtracted correct length";
for -1..1 {
    $left = $_;
    dies-ok -> {Behavior.new($p, :$left, :!free)}, "Behavior short/bad length $_ detected";
}
is +DefaultBehavior, 0, "PermanentLockBehavior numifies to its subcode";
$ev = CArray[uint8].new(1,2); $p = nativecast(Pointer[uint8], $ev);
$left = 2;
$n = CommonBehavior.new($p, :$left, :!free);
ok  $n.isa(LockBehavior), "Made a LockBehavior from bytes (CommonBehavior.new)";
ok  $n.does(Behavior), "LockBehavior does Behavior";
$left = 2;
$n = Behavior.new($p, :$left, :!free);
ok  $n.isa(LockBehavior), "Made a LockBehavior from bytes (Behavior.new)";
is +LockBehavior, 1, "LockBehavior numifies to its subcode";
$ev = CArray[uint8].new(129,2); $p = nativecast(Pointer[uint8], $ev);
$left = 2;
$n = CommonBehavior.new($p, :$left, :!free);
ok  $n.isa(PermamentLockBehavior), "Made a PermamentLockBehavior (sic) from bytes (CommonBehavior.new)";
ok  $n.does(Behavior), "PermamentLockBehavior (sic) does Behavior";
$left = 2;
$n = Behavior.new($p, :$left, :!free);
ok  $n.isa(PermamentLockBehavior), "Made a PermamentLockBehavior (sic) from bytes (Behavior.new)";
is +PermamentLockBehavior, 129, "PermamentLockBehavior (sic) numifies to its subcode";
$ev = CArray[uint8].new(3,42); $p = nativecast(Pointer[uint8], $ev);
$left = 2;
$n = CommonBehavior.new($p, :$left, :!free);
ok  $n.isa(Overlay1Behavior), "Made a Overlay1Behavior from bytes (CommonBehavior.new)";
ok  $n.does(OverlayBehavior), "Overlay1Behavior does OverlayBehavior";
ok  $n.does(Behavior), "Overlay1Behavior does Behavior";
$left = 2;
$n = Behavior.new($p, :$left, :!free);
ok  $n.isa(Overlay1Behavior), "Made a Overlay1Behavior from bytes (Behavior.new)";
is $n.key, 42, "Overlay1Behavior has correct .key";
is +Overlay1Behavior, 3, "Overlay1Behavior numifies to its subcode";
$o = PermamentOverlay1Behavior.new(:43key);
ok $o.isa(PermamentOverlay1Behavior), "Made an PermamentOverlay1Behavior (sic) by hand";
ok  $o.does(PermamentOverlayBehavior), "PermamentOverlay1Behavior (sic) does PermamentOverlayBehavior";
ok  $o.does(Behavior), "PermamentOverlay1Behavior (sic) does Behavior";
is $o.key, 43, "Hand made PermamentOverlay1Behavior all fields have expected values";
is (|(.values for $o.bufs)).flat[^2], (131,43), "Hand made PermamentOverlay1Behavior serializes to correct bytes";
is &Behavior(130), PermamentRadioGroupBehavior, "PermamentRadioGroupBehavior coercer with Int";
is &Behavior(PermamentRadioGroupBehavior), PermamentRadioGroupBehavior, "Behavior coercer idempotence";
is &Behavior()[1].gist, Pair.new("1",LockBehavior).gist, "Behavior list works";


#my $fabnames = X11::XCB::xkb::GetNamesReply.new(sequence => 3, deviceID => 3, which => (
#    NameDetail::KeycodesMask, ::GetNamesReply::valueList::NameDetail::KeycodesMask.new(keycodesName => 160),
#    NameDetail::GeometryMask, ::GetNamesReply::valueList::NameDetail::GeometryMask.new(geometryName => 0),
#    NameDetail::SymbolsMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::SymbolsMask.new(symbolsName => 502),
#    NameDetail::PhysSymbolsMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::PhysSymbolsMask.new(physSymbolsName => 502),
#    NameDetail::TypesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::TypesMask.new(typesName => 161),
#    NameDetail::CompatMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::CompatMask.new(compatName => 161),
#    NameDetail::KeyTypeNamesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::KeyTypeNamesMask.new(typeNames => [162, 164, 167, 169, 171, 173, 174, 176, 177, 178, 179, 180, 181, 185, 192, 193, 198, 199, 200, 201, 202, 203, 204, 205, 208, 210]),
#    NameDetail::KTLevelNamesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::KTLevelNamesMask.new(:ktLevelNames([[1], [2]], [3])),
#    NameDetail::IndicatorNamesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::IndicatorNamesMask.new(
#        :indicatorNames(1,2,3,4,5,6,7,8,9,10,11,12,Nil,14,15,16,17,18,19,20,21,21,23,24,25,26,27,28,29,30,31,32)),
#    NameDetail::KeyNamesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::KeyNamesMask.new(
#        :keyNames(X11::XCB::xkb::KeyName.new(name => "Kfee"),
#                  X11::XCB::xkb::KeyName.new(name => "Kfie"))),
#    NameDetail::KeyAliasesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::KeyAliasesMask.new(
#        :keyAliases(X11::XCB::xkb::KeyAlias.new(real => "Kfoo", alias => "Kfum"))),
#    NameDetail::VirtualModNamesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::VirtualModNamesMask.new(
#        :virtualModNames(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)),
#    NameDetail::GroupNamesMask, X11::XCB::xkb::GetNamesReply::valueList::NameDetail::GroupNamesMask.new(
#        :groups => (1,2,3,4,5,6,7,Nil))),
#
#    minKeyCode => 8,
#    maxKeyCode => 45,
#    nTypes => 26,
#    groupNames => 3,
#    virtualMods => 8191,
#    firstKey => 8,
#    nKeys => -8,
#    indicators => 16383,
#    nRadioGroups => 0,
#    nKeyAliases => 33,
#    nKTLevels => 96);

#my $c = Connection.new;

#my $ue = UseExtensionRequest.new(:wantedMajor(1),:wantedMinor(0));
#$ue.send($c);
#my $rq = GetStateRequest.new(:deviceSpec(X11::XCB::xkb::IDEnum::UseCoreKbd));
#my $rq = GetNamesRequest.new(:deviceSpec(X11::XCB::xkb::IDEnum::UseCoreKbd), :which(0x1fff));
##                        :which(X11::XCB::xkb::NameDetailEnum::KTLevelNamesMask));
##my $rq = SetDebuggingFlagsRequest.new(:affectFlags(1), :flags(1), :affectCtrls(1), :ctrls(1), :message<testfeefiefoo>);
#$rq.perl.say;
#
#my $rp = $rq.send($c);
#my $names = await($rp).receive;
#
#$names.perl.say;

CATCH { $_.message.say };


