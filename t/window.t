use v6;
use lib <blib/lib lib>;

use Test;

plan 8;

use NativeCall;
use X11;
use nqp;

my $c = Connection.new;

use X11::XCB::XProto;

my $wid = Resource.new(:from($c));

my $cw = CreateWindowRequest.new(
   :wid($wid.value),
   :depth(24),
   :parent($c.roots[0].root),
   :x(100),
   :y(100),
   :width(250),
   :height(250),
   :border_width(10),
   :class(1),
   :visual($c.roots[0].root_visual)
   :value_list{"16" => 1, "2" => 0x00ffffff, "8" => 0, "2048" => 4325376, "8192" => 0x20 }
);

my $voidpromise = $cw.send($c);
ok $voidpromise.sequence, "Sent a CreateWindow Request";
my $broken = 0;
my $voidwait = start {
    await($voidpromise);
    CATCH { default { $broken = 1; .resume } } 
};

my $qt = QueryTreeRequest.new(:window($wid.value));
my $cookie = $qt.send($c);
$c.flush;
my $res = await($cookie).receive;
ok $res ~~ QueryTreeReply, "Got a QueryTree reply";
ok $res.root == $res.parent == $c.roots[0].root, "QueryTree reply has correct root/parent IDs";
await $voidwait;
ok $broken, "Original CreateWindow gets a broken cookie";

my $dw = DestroyWindowRequest.new(:window($wid.value));
$dw.send($c);
$c.flush;
$qt = QueryTreeRequest.new(:window($wid.value));
$cookie = $qt.send($c);
$c.flush;
$broken = 0;
$voidwait = start { my $res = await($cookie);
  CATCH { default { $broken = 1; .resume } }
}
# Send something else that will get a reply
ListExtensionsRequest.new.send($c);
$c.flush;
await($voidwait);
ok $broken, "QueryTree after DestroyWindow gets broken cookie";

my $w = Window.new($c, :!map);
ok $w ~~ Window, "Made a window through high level object";
$qt = QueryTreeRequest.new(:window($w.wid.value));
$cookie = $qt.send($c);
$c.flush;
$res = await($cookie).receive;
ok $res ~~ QueryTreeReply, "Got another QueryTree reply";
$w = Nil;
{ use nqp;
  nqp::force_gc; sleep 0.1;
  nqp::force_gc; sleep 0.1;
  nqp::force_gc; sleep 0.1;
}
$qt = QueryTreeRequest.new(:window($wid.value));
$cookie = $qt.send($c);
$c.flush;
$broken = 0;
$voidwait = start { my $res = await($cookie);
  CATCH { default { $broken = 1; .resume } }
}
# Send something else that will get a reply
ListExtensionsRequest.new.send($c);
$c.flush;
await($voidwait);
ok $broken, "QueryTree after DESTROY gets broken cookie";