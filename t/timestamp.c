use v6;
use lib <blib/lib lib>;

use Test;

plan 8;

use X11 :internal;
use X11::XCB::XProto;

my $t1 = TimeStamp.new(1);
is +$t1, 1, 'Made TimeStamp and Numifies OK';
my $t2 = TimeStamp.new(2);
is $t2 - $t1, 1, 'Simple timestamp math, positive';
is $t1 - $t2, -1, 'Simple timestamp math, negative';
is TimeStamp.new(42) - TimeStamp.new(42), 0, 'Simple timestamp math, equal';
is $t2 + 1, 3, 'Simple add Int to timestamp';
is ++$t2, 3, 'Simple timestamp increment';

is ({ TimeStamp.new($_) - $t1 } for 0x7fffffff..0x80000003),
   (0x7ffffffe, 0x7fffffff, 0x80000000, -0x7fffffff, -0x7ffffffe),
   'Wrapping math left';

$t1 = TimeStamp.new(0xffffffff);

is ({ $t1 - TimeStamp.new($_) } for 0x7ffffffd..0x80000001),
   (-0x7ffffffe, -0x7fffffff, 0x80000000, 0x7fffffff, 0x7ffffffe),
   'Wrapping math right';

