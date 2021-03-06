use v6;
use lib <blib/lib lib>;

use Test;

plan 14;

use X11;
use X11::XCB::Sync;

my $c = Connection.new;
my $lsc = ListSystemCountersRequest.new;
ok $lsc.isa(ListSystemCountersRequest), "Made a ListSystemCountersRequest";

my $cookie = $lsc.send($c);
my $res = await($cookie).receive;

ok $res.isa(ListSystemCountersReply), "Got a ListSystemCountersReply";
my $st = $res.counters.first(*.name eq "SERVERTIME");
ok $st.isa(Systemcounter), "Found a SERVERTIME Systemcounter";

my $qc = QueryCounterRequest.new(:counter($st.counter));
ok $qc.isa(QueryCounterRequest), "Made a QueryCounterRequest";
$cookie = $qc.send($c);
my $qcr = await($cookie).receive;
ok $qcr.isa(QueryCounterReply), "Got a QueryCounterReply";

my $before = $qcr.counter_value;
sleep(0.1);
$cookie = $qc.send($c);
$qcr = await($cookie).receive;
my $after = $qcr.counter_value;

$before = (($before.hi +& 0xffffffff) +< 32) +| ($before.lo +& 0xffffffff);
$after = (($after.hi +& 0xffffffff) +< 32) +| ($after.lo +& 0xffffffff);

my $alarmid = Resource.new(:from($c));
my $car = CreateAlarmRequest.new(:id($alarmid.value), :value_mask(
    CAEnum::Counter, CreateAlarmRequest::value_list::CA::Counter.new(:counter(4242)),
    CAEnum::ValueType, CreateAlarmRequest::value_list::CA::ValueType.new(:valueType(VALUETYPEEnum::Absolute)),
    CAEnum::TestType, CreateAlarmRequest::value_list::CA::TestType.new(:testType(TESTTYPEEnum::PositiveComparison)),
    CAEnum::Events, CreateAlarmRequest::value_list::CA::Events.new(:events(1))
    ));

is $car.counter, $car.value_mask{CAEnum::Counter}.counter, "punch through method read access";
lives-ok { $car.counter = 4343 }, "punch through method write access lives";
is $car.counter, 4343, "punch through method write access worked";
lives-ok { $car.counter = CreateAlarmRequest::value_list::CA::Counter }, "punch through method deletion mechanism lives";
nok $car.value_mask{CAEnum::Counter}:exists, "punch through method deletion mechanism works";
lives-ok { $car.counter = 4444 }, "punch through method autovivify mechanism lives";
is $car.counter, 4444, "punch through method autovivify access worked";

# TODO: get these test to run as fast as possible without flapping
ok $after > $before, "Counter is running fast enough to try a trigger";
if $after > $before {
    my $dt = $after - $before;
    $dt *= 5;
    $dt += $after;
    my $at = Counter64.new(:hi($dt +> 32), :lo($dt +& 0xffffffff));

    $car.counter = $st.counter;
    $car.value = $at;

    my $w = Channel.new;
    $c.watch($w);
    my $oldc = $w.receive;
    my $ev;

    my $t = start {
        $ev = $w.receive;
    }
    $cookie = $car.send($c);
    await Promise.anyof($t, start { sleep 1 });
    ok $ev.isa(SyncAlarmNotifyEvent), "Got a SyncAlarmNotifyEvent";
}

CATCH { $_.message.say };