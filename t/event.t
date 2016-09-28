use v6;
use lib <blib/lib lib>;

use Test;

plan 1;

use NativeCall;
use X11;
use X::Protocol::X11;

my $c = Connection.new;

my Channel $w .= new;
$c.watch.send($w);

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
   :value_list{"16" => 1, "2" => 0x00ffffff, "8" => 0, "2048" => 0x420000, "8192" => 0x20 }
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
$c.watch.send($w2).WHICH;
is $w.receive.WHICH, $w.WHICH, "Got old channel back when replacing";
$se.send($c);
$res = $w2.receive;
# finagle the sequence number
$cme.sequence = $res.sequence;
is-deeply $res, $cme, "Received event from SendEvent on new channel";
