use v6;
use lib <blib/lib lib>;

use Test;

plan 6;

use NativeCall;
use X11;
use nqp;

my $c = Connection.new;
my $r1 = Resource.new(:from($c));
ok ($r1.defined and $r1 ~~ Resource), "Got a resource";
ok ($r1.from === $c), "Resource holds reference to its Connection";
my $r2 = Resource.new(:from($c));
my $v2 = $r2.value;
ok $r1.value != $v2, "Second resource is unique";
$r2 = Nil;
for 1..5 { sleep 0.1; nqp::force_gc; Rat.new }
$r2 = Resource.new(:from($c));
ok $v2 == $r2.value, "Resource value was recycled";
my $r3 = Resource.new(:from($c));
ok $r1.value != $r2.value != $r3.value, "New unique value gotten";

$c = Nil;
$r1 = Nil;
$r2 = Nil;
$r3 = Nil;

$c = Nil;
$r1 = Nil;
$r2 = Nil;
$r3 = Nil;

# Above GC runs probably promoted $c.  Use another connection.
$c = Connection.new;
$r1 = Resource.new(:from($c));
my $ask = $c.res_ask;
my $scrap = $c.res_scrap;
$c = Nil;
$r1 = Nil;

my $destroyed = 0;
start {
   await $ask.closed, $scrap.closed;
   $destroyed = 1;
}
for 1..5 { sleep 0.1; nqp::force_gc; Rat.new };
ok $destroyed, "Resource server stops when connection is destroyed";
