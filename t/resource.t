use v6;
use lib <blib/lib lib>;

use Test;

plan 4;

use NativeCall;
use X11;
use nqp;

my $c = Connection.new;
my $r1 = Resource.new(:from($c));
ok ($r1.defined and $r1 ~~ Resource), "Got a resource";
my $r2 = Resource.new(:from($c));
my $v2 = $r2.value;
ok $r1.value != $v2, "Second resource is unique";
$r2 = Nil;
nqp::force_gc;
sleep 0.1;
nqp::force_gc;
sleep 0.1;
nqp::force_gc;
sleep 0.1;
nqp::force_gc;
sleep 0.1;
$r2 = Resource.new(:from($c));
ok $v2 == $r2.value, "Resource value was recycled";
my $r3 = Resource.new(:from($c));
ok $r1.value != $r2.value != $r3.value, "New unique value gotten";
