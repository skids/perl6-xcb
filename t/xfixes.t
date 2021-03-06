use v6;
use lib <blib/lib lib>;

use Test;

plan 2;

use NativeCall;
use X11;
use X11::XCB::XProto;
import CWEnum :enums;
use X11::XCB::XFixes;

use nqp;

my $c = Connection.new;

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

$cw.send($c);

my $ci = XFixesQueryVersionRequest.new(:6client_major_version,:0client_minor_version);
my $cookie = $ci.send($c);
$c.flush;

my $res = await($cookie).receive;
ok $res ~~ XFixesQueryVersionReply, "Got a XFixesQueryVersionReply";

$ci = FetchRegionRequest.new(:region(0));
$cookie = $ci.send($c);
$c.flush;

# TODO: need X::Protocol::X11 subclasses
throws-like { await($cookie).receive }, X::Protocol::X11,
message => /:s X11 protocol error\: Bad Request/;


# TODO This sees broke.  perhaps all reqs with no fields.  look into it
# $ci = GetCursorImageRequest.new;

# TODO noninteractive testing of this
#import X11::XCB::XFixes::SelectionEventMaskEnum :enums;
#$ci = SelectSelectionInputRequest.new(
#    :window($wid.value),
#    :selection(AtomEnum::PRIMARY),
#    :event_mask([+|] SetSelectionOwnerMask,
#                     SelectionWindowDestroyMask,
#                     SelectionClientCloseMask)
#);
#$cookie = $ci.send($c);
#$c.flush;
#start {
#    my $events = $c.watch;
#    $events.receive.perl.say;
#    loop {
#        $events.receive.perl.say;
#    }
#}
#sleep 10;



