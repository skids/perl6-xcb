use v6;
use lib <blib/lib lib>;

use Test;

plan 10;

use NativeCall;
my $errno := cglobal('libc.so.6', 'errno', int32);
my sub mmap(Pointer $addr, size_t $length, int32 $prot, int32 $flags,
            int32 $fd, size_t $offset --> Pointer) is native { * };
my sub munmap(Pointer $addr, size_t $length) is native { * };

use X11;
use X11::XCB::XProto;
import CWEnum :enums;
use X11::XCB::Shm;

my $c = Connection.new;

my $ci = ShmQueryVersionRequest.new(:6client_major_version,:0client_minor_version);
my $cookie = $ci.send($c);
$c.flush;

my $res = await($cookie).receive;
ok $res ~~ ShmQueryVersionReply, "Got a ShmQueryVersionReply";

my $shmseg = Resource.new(:from($c));
my $cs = CreateSegmentRequest.new(:65536size, :shmseg($shmseg.value), :read_only(0));
$cookie = $cs.send($c);
$res = await($cookie).receive;
ok $res ~~ CreateSegmentReply, "Got a CreateSegmentReply";
ok $res.shm_fd > 0, "File descriptor is positive";
my $addr;

$addr = mmap(Pointer,65536,3,1,$res.shm_fd,0);

ok ($addr !=:= Pointer.new(-1) or $errno == 0), "We seem to have mmaped a SHM";

my $buf = nativecast(CArray[uint8], $addr);
# Put some unlikely data in the buffer.
$buf[0] = 0x5a;
$buf[1] = 0x76;
$buf[2] = 0x3c;
$buf[3] = 0xc3;

my $before = $buf[0..3].gist;

my $gi = ShmGetImageRequest.new(
    :drawable($c.roots[0].root), :0x, :0y, :2width, :1height,
    :16777215plane_mask , :format(X11::XCB::XProto::ImageFormatEnum::ZPixmap), :shmseg($shmseg.value), :0offset);

$cookie = $gi.send($c);
$res = await($cookie).receive;
ok $res ~~ ShmGetImageReply, "Got a ShmGetImageReply";
isnt $buf[0..3].gist, $before, "ShmGetImage fiddled with our buffer data";

my $sd = DetachRequest.new(:shmseg($shmseg.value));
$sd.send($c);

munmap($addr, 65536);

use File::Temp;

my ($fn, $fh) = |tempfile;
my $fd = $fh.native-descriptor;
$fh.print((0x5a,0x76,0x3c,0xc3).chrs x 16384);
$addr = mmap(Pointer,65536,3,1,$fd,0);
ok ($addr !=:= Pointer.new(-1) or $errno == 0), "mmaped a file locally";
$buf = nativecast(CArray[uint8], $addr);
is $buf[0..3].gist, $before, "Local file prepped with correct data";

$shmseg = Resource.new(:from($c));

my $af = AttachFdRequest.new(:shmseg($shmseg.value), :0read_only, :shm_fd($fd));
$cookie = $af.send($c);

$gi = ShmGetImageRequest.new(
    :drawable($c.roots[0].root), :0x, :0y, :2width, :1height,
    :16777215plane_mask , :format(X11::XCB::XProto::ImageFormatEnum::ZPixmap), :shmseg($shmseg.value), :0offset);
$cookie = $gi.send($c);
$res = await($cookie).receive;
ok $res ~~ ShmGetImageReply, "Got a ShmGetImageReply (AttachFd)";
isnt $buf[0..3].gist, $before, "ShmGetImage fiddled with our buffer data (AttachFd)";

$sd = DetachRequest.new(:shmseg($shmseg.value));
$sd.send($c);

munmap($addr, 65536);
$fh.close;
