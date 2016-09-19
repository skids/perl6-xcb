use v6;
use lib <blib/lib lib>;

use Test;

plan 9;

use NativeCall;
use X11;
use nqp;

my $c = Connection.new;

# Test X core Fonts.  These are a deprecated X11 feature, but we
# test them because they stress us with huge arrays of nested
# structures and also have a couple peculiarities:
#
# 1) Single nested data structures before normal fields
# 2) A need to run a command that has no response

use X11::XCB::XProto;

my $font = Font.new($c, :first<*-fixed-*>);
ok ($font ~~ Font and $font.defined), "Got a font through top level api.";

my $lf = ListFontsRequest.new(:max_names(100), :pattern<*-hoopiedoop-*>);
my $fl = await($lf.send($c)).list[0];
ok $fl ~~ ListFontsReply, "Got a reply to a nonsense ListFontsRequest";
ok $fl.names.elems == 0, "Empty reply to ListFontsRequest works";
$lf = ListFontsRequest.new(:max_names(100), :pattern<*-fixed-*>);
$fl = await($lf.send($c)).list[0];
ok $fl ~~ ListFontsReply, "Got a reply to a serious ListFontsRequest";
ok $fl.names.elems > 1, "Got fonts in a ListFontsRequest";
ok $fl.names[0] ~~ Str, "Font names are Strs";
ok $fl.names[0] ~~ /fixed/, "A font name has expected content";

my $fid = Resource.new(:from($c));
my $of = OpenFontRequest.new(:fid($fid.value), :name($fl.names[0]));
my $voidpromise = $of.send($c);
my $broken = 0;
my $voidwait = start {
    await($voidpromise);
    CATCH { default { $broken = 1; .resume } } 
};
my $qf = QueryFontRequest.new(:font($fid.value));
my $p = $qf.send($c);
$lf = ListFontsRequest.new(:max_names(100), :pattern<*-fixed-*>);
my $lfc = $lf.send($c);
$fl = await($lfc).list[0];
ok $fl ~~ ListFontsReply,
    "Did a ListFontsRequestwhile QueryFont was building object";
ok await($p).receive.char_infos[*-1] ~~ Charinfo,
    "Got Font and it has array of Charinfo";
await $voidwait;
ok $broken, "OpenFontRequest got a broken cookie";
