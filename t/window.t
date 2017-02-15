use v6;
use lib <blib/lib lib>;

use Test;

plan 17;

use NativeCall;
use X11;
use nqp;

my $c = Connection.new;

use X11::XCB::XProto;
import CWEnum :enums;

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
   :value_list(CWBitGravity,  1,
               CWBackPixel,   0x00ffffff,
               CWBorderPixel, 0,
               CWEventMask,   4325376,
               CWColormap,    0x20,
              )
);

my $voidpromise = $cw.send($c);
ok $voidpromise.sequence, "Sent a CreateWindow Request";
my $broken = 0;
my $voidwait = start {
    await($voidpromise);
    CATCH { default { $broken = 1; } }
};

my $qt = QueryTreeRequest.new(:window($wid.value));
my $cookie = $qt.send($c);
$c.flush;
my $res = await($cookie).receive;
ok $res ~~ QueryTreeReply, "Got a QueryTree reply";
ok $res.root == $res.parent == $c.roots[0].root, "QueryTree reply has correct root/parent IDs";
await $voidwait;
ok $broken, "Original CreateWindow gets a broken cookie";

# test properties while we have it open
my $wp = ListPropertiesRequest.new(:window($wid.value));
$cookie = $wp.send($c);
$c.flush;
$res = await($cookie).receive;
ok $res ~~ ListPropertiesReply, "Got a ListProperty reply";
my $oatoms = set($res.atoms);
my $cp = ChangePropertyRequest.new(:window($wid.value),
    :property(AtomEnum::COPYRIGHT), :type(AtomEnum::STRING),
    :format(8), :mode(PropModeEnum::Replace), :data(array[uint8].new(^256)));
$cp.send($c);
$cookie = $wp.send($c);
$c.flush;
$res = await($cookie).receive;
ok $res ~~ ListPropertiesReply, "Got a second ListProperty reply";
is set($res.atoms) (-) $oatoms, set(+AtomEnum::COPYRIGHT), "New property is listed";
my $gp = GetPropertyRequest.new(:window($wid.value), :!delete,
    :property(AtomEnum::COPYRIGHT), :type(AtomEnum::STRING),
    :long_offset(32), :long_length(16));
$cookie = $gp.send($c);
$c.flush;
$res = await($cookie).receive;
ok $res ~~ GetPropertyReply, "Got a GetPropertyReply";
is-deeply $res.value, array[uint8].new(128..^192), "Data in GetPropertyReply looks correct";
is $res.bytes_after, 64, "Correct unread data amount in GetPropertyReply";
$gp.delete = True;
$gp.long_length = 32;
$cookie = $gp.send($c);
$c.flush;
my $res2 = await($cookie).receive;
is-deeply $res.value[*], $res2.value[^64], "Second GetPropertyReply, with delete, returns same data";
$cookie = $wp.send($c);
$c.flush;

# TODO: test DeleteProperty, RotateProperties, PropertyNotify

is-deeply set(await($cookie).receive.atoms), $oatoms, "Original list of properties restored";
my $dw = DestroyWindowRequest.new(:window($wid.value));
$dw.send($c);
$c.flush;
$qt = QueryTreeRequest.new(:window($wid.value));
$cookie = $qt.send($c);
$c.flush;
$broken = 0;
$voidwait = start { my $res = await($cookie);
  CATCH { default { $broken = 1; } }
}
# Send something else that will get a reply
ListExtensionsRequest.new.send($c);
$c.flush;
await($voidwait);
ok $broken, "QueryTree after DestroyWindow gets broken cookie";
# Test async API and confirm error message
throws-like { $qt.demand($c) },
    X::Protocol::X11, message => /:s X11 protocol error\: Bad Window/;


# Higher level API
my $w = Window.new($c, :!map);
ok $w ~~ Window, "Made a window through high level object";
$qt = QueryTreeRequest.new(:window($w.wid.value));
$res = $qt.demand($c);
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
  CATCH { default { $broken = 1; } }
}
# Send something else that will get a reply
ListExtensionsRequest.new.send($c);
$c.flush;
await($voidwait);
ok $broken, "QueryTree after DESTROY gets broken cookie";
