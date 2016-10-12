use v6;
use lib <blib/lib lib>;

use Test;

plan 96;

use X11;
use X11::XCB::XProto;

my $nxname = "NX_NAME_DOES_NOT_EXIST" ~ now.Int;
my $nxnum = now.Int +& 0xffffffff +| 0xc0000000;

# Static atom pairs
my $primary = AtomPair.new(:name<PRIMARY>);
ok $primary ~~ AtomPair, "Can create known-good static AtomPair (name only)";
ok $primary.from === $primary.xcb  === AtomEnum::Atom,
   "Static atom (name only) owned by AtomEnum::Atom";
is $primary.name, "PRIMARY", "Static AtomPair (name only) name is correct";
is $primary.value, 1, "Static AtomPair (name only) value is correct";

my $nxn = AtomPair.new(:name($nxname));
is $nxn.name, $nxname, "Invalid static AtomPair (name only) name is correct";
nok $nxn.value.defined, "Invalid static AtomPair (name only) value is undefined";
nok $nxn.valid, "Invalid static AtomPair (name only) is not .valid";
throws-like { +$nxn }, X::X11::NoSuchAtomPair,
    message => "No static atom named \"$nxname\"";

my $secondary = AtomPair.new(:2value);
ok $secondary ~~ AtomPair, "Can create known-good static AtomPair (value only)";
ok $secondary.from === $primary.xcb === AtomEnum::Atom,
   "Static atom (value only) owned by AtomEnum::Atom";
is $secondary.name, "SECONDARY", "Static AtomPair (value only) name is correct";
is $secondary.value, 2, "Static AtomPair (value only) value is correct";

my $nxa = AtomPair.new(:value($nxnum));
is $nxa.value, $nxnum, "Invalid static AtomPair (value only) value is correct";
nok $nxa.name.defined, "Invalid static AtomPair (value only) name is undefined";
nok $nxa.valid, "Invalid static AtomPair (value only) is not .valid";
throws-like { +$nxa }, X::X11::NoSuchAtomPair, message => "No static atom #$nxnum";

my $atomatom = AtomPair.new(:4value, :name<ATOM>);
ok $atomatom ~~ AtomPair, "Can create known-good static AtomPair (value and name)";
ok $atomatom.from === $primary.xcb === AtomEnum::Atom,
   "Static atom (value only) owned by AtomEnum::Atom";
is $atomatom.name, "ATOM", "Static AtomPair (value and name) name is correct";
is $atomatom.value, 4, "Static AtomPair (value and name) value is correct";

$nxa = AtomPair.new(:name<ATOM>, :value($nxnum));
is $nxa.value, $nxnum, "Invalid static AtomPair (name and invalid value) value is correct";
is $nxa.name, "ATOM", "Invalid static AtomPair (name and invalid value) name is correct";
throws-like { +$nxa }, X::X11::NoSuchAtomPair, message => "No static atom #$nxnum named \"ATOM\"";
nok $nxa.valid, "Invalid static AtomPair (name and invalid value) is not .valid";


$nxa = AtomPair.new(:value(4), :name($nxname));
is $nxa.value, 4, "Invalid static AtomPair (value and invalid name) value is correct";
is $nxa.name, $nxname, "Invalid static AtomPair (value and invalid name) name is correct";
throws-like { +$nxa }, X::X11::NoSuchAtomPair, message => "No static atom #4 named \"$nxname\"";
nok $nxa.valid, "Invalid static AtomPair (value and invalid name) is not .valid";

throws-like {AtomPair.new()}, X::AdHoc, message => /:s X11\:\:AtomPair\.new usage\: Create a new AtomPair/;

my $c = Connection.new;

# Static atom pairs, owned by a connection
$primary = AtomPair.new($c, :name<PRIMARY>);
ok $primary ~~ AtomPair, "Can create known-good static+owned AtomPair (name only)";
is-deeply $primary.from, $c,
   "Static+owned AtomPair (name only) correct connection";
is-deeply $primary.xcb, $c.xcb,
   "Static+owned AtomPair (name only) correct xcb";
is $primary.name, "PRIMARY", "Static+owned AtomPair (name only) name is correct";
is $primary.value, 1, "Static+owned AtomPair (name only) value is correct";

$secondary = AtomPair.new($c, :2value);
ok $secondary ~~ AtomPair, "Can create known-good static+owned AtomPair (value only)";
is-deeply $secondary.from, $c,
   "Static+owned AtomPair (value only) correct connection";
is-deeply $secondary.xcb, $c.xcb,
   "Static+owned AtomPair (value only) correct xcb";
is $secondary.name, "SECONDARY", "Static+owned AtomPair (value only) name is correct";
is $secondary.value, 2, "Static+owned AtomPair (value only) value is correct";

$atomatom = AtomPair.new($c, :4value, :name<ATOM>);
ok $atomatom ~~ AtomPair, "Can create known-good static+owned AtomPair (value and name)";
is-deeply $atomatom.from, $c,
   "Static+owned AtomPair (value and name) correct connection";
is-deeply $atomatom.xcb, $c.xcb,
   "Static+owned AtomPair (value and name) correct xcb";
is $atomatom.name, "ATOM", "Static+owned AtomPair (value and name) name is correct";
is $atomatom.value, 4, "Static+owned AtomPair (value and name) value is correct";

throws-like {AtomPair.new($c)}, X::Multi::NoMatch;

my $incr = AtomPair.new($c, :name<INCR>);
ok $incr ~~ AtomPair, "Can create (hopefully) known-good dynamic AtomPair (name only)";
is-deeply $incr.from, $c,
   "(hopefully) known-good dynamic AtomPair correct owner pre-sync";
is-deeply $incr.xcb, $c.xcb,
   "(hopefully) known-good dynamic AtomPair correct xcb pre-sync";
nok $incr.valid, "(hopefully) known-good dynamic AtomPair (name only) not valid yet";
ok $incr.valid(:sync), "(hopefully) known-good dynamic AtomPair (name only) valid after sync";
is-deeply $incr.from, $c,
   "(hopefully) known-good dynamic AtomPair correct owner post-sync";
is-deeply $incr.xcb, $c.xcb,
   "(hopefully) known-good dynamic AtomPair correct xcb post-sync";

$nxn = AtomPair.new($c, :name($nxname));
is $nxn.name, $nxname, "Invalid dynamic AtomPair (name only) name is correct";
nok $nxn.value.defined, "Invalid dynamic AtomPair (name only) value is undefined";
nok $nxn.valid, "Invalid dynamic AtomPair (name only) is not .valid";
is-deeply $nxn.from, $c,
   "Invalid dynamic atom (name only) correct connection";
is-deeply $nxn.xcb, $c.xcb,
   "Invalid dynamic atom (name only) correct xcb";
throws-like { +$nxn }, X::X11::NoSuchAtomPair, message => "No dynamic atom named \"$nxname\"";

# It is pure coincidence the first dynamic atom is #69, I am sure.
my $dude69 = AtomPair.new($c, :value(69));
ok $dude69 ~~ AtomPair, "Can create (hopefully) known-good dynamic AtomPair (value only)";
is-deeply $dude69.from, $c,
   "(hopefully) known-good dynamic AtomPair correct owner pre-sync";
is-deeply $dude69.xcb, $c.xcb,
   "(hopefully) known-good dynamic AtomPair correct xcb pre-sync";
nok $dude69.valid, "(hopefully) known-good dynamic AtomPair (value only) not valid yet";
ok $dude69.valid(:sync), "(hopefully) known-good dynamic AtomPair (value only) valid after sync";
is-deeply $dude69.from, $c,
   "(hopefully) known-good dynamic (value only) AtomPair correct owner post-sync";
is-deeply $dude69.xcb, $c.xcb,
   "(hopefully) known-good dynamic (value only) AtomPair correct xcb post-sync";

# It is pure coincidence the first dynamic atom is #69, I am sure.
$dude69 = AtomPair.new($c, :value(69), :name($dude69.name));
ok $dude69 ~~ AtomPair, "Can create (hopefully) known-good dynamic AtomPair (name and value)";
is-deeply $dude69.from, $c,
   "(hopefully) known-good dynamic AtomPair correct owner pre-sync";
is-deeply $dude69.xcb, $c.xcb,
   "(hopefully) known-good dynamic AtomPair correct xcb pre-sync";
nok $dude69.valid, "(hopefully) known-good dynamic AtomPair (name and value) not valid yet";
ok $dude69.valid(:sync), "(hopefully) known-good dynamic AtomPair (name and value) valid after sync";
is-deeply $dude69.from, $c,
   "(hopefully) known-good dynamic (name and value) AtomPair correct owner post-sync";
is-deeply $dude69.xcb, $c.xcb,
   "(hopefully) known-good dynamic (name and value) AtomPair correct xcb post-sync";

$nxa = AtomPair.new($c, :value($nxnum));
is $nxa.value, $nxnum, "Invalid dynamic AtomPair (value only) value is correct";
nok $nxa.name.defined, "Invalid dynamic AtomPair (value only) name is undefined";
nok $nxa.valid, "Invalid dynamic AtomPair (value only) is not .valid";
is-deeply $nxa.from, $c,
   "Invalid dynamic atom (value only) correct connection";
is-deeply $nxa.xcb, $c.xcb,
   "Invalid dynamic atom (value only) correct xcb";
throws-like { +$nxa }, X::Protocol::X11;

$nxa = AtomPair.new($c, :name<ATOM>, :value($nxnum));
is $nxa.value, $nxnum, "Invalid dynamic AtomPair (name and invalid value) value is correct";
is $nxa.name, "ATOM", "Invalid dynamic AtomPair (name and invalid value) name is correct";
is-deeply $nxa.from, $c,
   "Invalid dynamic atom (name and invalid value) correct connection";
is-deeply $nxa.xcb, $c.xcb,
   "Invalid dynamic atom (name and invalid value) correct xcb";
throws-like { +$nxa }, X::X11::NoSuchAtomPair, message => "No static atom #$nxnum named \"ATOM\"";
nok $nxa.valid, "Invalid dynamic AtomPair (name and invalid value) is not .valid";

$nxa = AtomPair.new($c, :name<INCR>, :value($nxnum));
is $nxa.value, $nxnum, "Invalid dynamic AtomPair (name and invalid value) value is correct";
is $nxa.name, "INCR", "Invalid dynamic AtomPair (name and invalid value) name is correct";
is-deeply $nxa.from, $c,
   "Invalid dynamic atom (name and invalid value) correct connection";
is-deeply $nxa.xcb, $c.xcb,
   "Invalid dynamic atom (name and invalid value) correct xcb";
throws-like { +$nxa }, X::Protocol::X11;
nok $nxa.valid, "Invalid dynamic AtomPair (name and invalid value) is not .valid";

$nxa = AtomPair.new($c, :value(4), :name($nxname));
is $nxa.value, 4, "Invalid dynamic AtomPair (value and invalid name) value is correct";
is $nxa.name, $nxname, "Invalid dynamic AtomPair (value and invalid name) name is correct";
is-deeply $nxa.from, $c,
   "Invalid dynamic atom (invalid and invalid name) correct connection";
is-deeply $nxa.xcb, $c.xcb,
   "Invalid dynamic atom (invalid and invalid name) correct xcb";
throws-like { +$nxa }, X::X11::NoSuchAtomPair, message => "Atom #4 is named ATOM not $nxname";
nok $nxa.valid, "Invalid dynamic AtomPair (value and invalid name) is not .valid";
