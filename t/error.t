use v6;
use lib <blib/lib lib>;

use Test;

plan 2;

use NativeCall;
use X11;
use X::Protocol::X11;

my $c = Connection.new;

use X11::XCB::XProto;

my $err;

throws-like { await(GetAtomNameRequest.new(:atom(65473)).send($c)) },
    X::Protocol::X11, message => /:s X11 protocol error\: Bad Atom/;

# Using syncronous API
throws-like { GetAtomNameRequest.new(:atom(65473)).demand($c) },
    X::Protocol::X11, message => /:s X11 protocol error\: Bad Atom/;
