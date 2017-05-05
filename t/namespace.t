use v6;
use lib <blib/lib lib>;

use Test;

plan 1;

eval-lives-ok '
use X11::XCB;
use X11::XCB::XProto;
use X11::XCB::BigRequests;
use X11::XCB::DPMS;
use X11::XCB::GenericEvent;
use X11::XCB::Record;
use X11::XCB::ScreenSaver;
use X11::XCB::XCMisc;
use X11::XCB::SELinux;
use X11::XCB::Xevie;
use X11::XCB::XPrint;
use X11::XCB::DRI3;
use X11::XCB::Res;
use X11::XCB::Shape;
use X11::XCB::XF86Dri;
use X11::XCB::Xinerama;

use X11::XCB::Present;
use X11::XCB::XFixes;
use X11::XCB::Render;
use X11::XCB::Sync;
use X11::XCB::Damage;
use X11::XCB::Composite;
use X11::XCB::Test;
use X11::XCB::Xv;
use X11::XCB::Shm;
use X11::XCB::XvMC;

use X11::XCB::RandR;
use X11::XCB::XF86VidMode;

# I do not think anyone would use DRI1 and DRI2 simultaneously
# and deconflicting them would be very hinky.
{
use X11::XCB::XF86Dri;
}
{
#use X11::XCB::DRI2;
}
use X11::XCB::xkb;
use X11::XCB::Glx;
use X11::XCB::Input;
die "Cursor overridden" unless Cursor.can("at");
die "Lock overridden" unless Lock.can("protect");
die "Any overridden" unless Any.^mro.gist eq "((Any) (Mu))";

', "All modules used with :DEFAULT";

