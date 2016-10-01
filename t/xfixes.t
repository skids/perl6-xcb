use v6;
use lib <blib/lib lib>;

use Test;

plan 2;

use NativeCall;
use X11;
use X11::XCB::XProto;
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
   :value_list{"16" => 1, "2" => 0x00ffffff, "8" => 0, "2048" => 4325376, "8192" => 0x20 }
);

$cw.send($c);

my $ci = XFixesQueryVersionRequest.new(:6client_major_version,:0client_minor_version);
my $cookie = $ci.send($c);
$c.flush;

my $res = await($cookie).receive;
$res.perl.say;
ok $res ~~ XFixesQueryVersionReply, "Got a XFixesQueryVersionReply";

$ci = FetchRegionRequest.new(:region(0));
$cookie = $ci.send($c);
$c.flush;

# TODO: need X::Protocol::X11 subclasses
throws-like { await($cookie).receive }, X::Protocol::X11,
message => /:s X11 protocol error\: Bad Request/;

# TODO This sees broke.  perhaps all reqs with no fields.  look into it
# $ci = GetCursorImageRequest.new;
