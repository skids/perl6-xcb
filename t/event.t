use v6;
use lib <blib/lib lib>;

use Test;

plan 9;

use NativeCall;
use X11;
use X::Protocol::X11;
use X11::XCB::XProto;
use nqp;

my $c = Connection.new;
my $wid = Resource.new(:from($c));

my Channel $w .= new;
$c.watch($w);

is $w.receive, False, "Got False back from first .watch call";

import CWEnum :enums;

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

$cw.send($c);

my $cme = ClientMessageEvent.new(:window($wid.value), :0type, :data(buf8.new(1..20)));
my $se = SendEventRequest.new(:destination($wid.value), :event_mask(0), :propagate(0), :event($cme));
$se.send($c);

my $res = $w.receive;
# finagle the sequence number
$cme.sequence = $res.sequence;
is-deeply $res, $cme, "Received event from SendEvent";

my Channel $w2 .= new;
$c.watch($w2);
is $w2.receive.WHICH, $w.WHICH, "Got old channel back when replacing";
$se.send($c);
$res = $w2.receive;
# finagle the sequence number
$cme.sequence = $res.sequence;
is-deeply $res, $cme, "Received event from SendEvent on new channel";
$c.unwatch;
await $w2.closed;
ok $w2.closed, "Channel gets closed when unwatching";
$se.send($c);
$w2 = Channel.new;
$c.watch($w2);
is $w2.receive, False, "Got False back from .watch after .unwatch";
my $async = 0;
$res = 0;
my $t = start {
    $res = $w2.receive;
    $async = 1;
}
sleep 0.1;
ok $async == 0, "Nothing was queued while unwatched";
$se.send($c);
await $t;
$cme.sequence = $res.sequence;
is-deeply $res, $cme, "Got event sent from another thread";

$c = Nil;
$cw = Nil;
$wid = Nil;

$c = Connection.new;
$wid = Resource.new(:from($c));
$cw = CreateWindowRequest.new(
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

$w .= new;
$c.watch($w);
$c = Nil;
$cw = Nil;
$wid = Nil;

for 1..10 { sleep 0.1; nqp::force_gc; Rat.new; };
throws-like { $w.send("foo") }, X::Channel::SendOnClosed, "event channel closed on destruction";

