perl6-xcb
========

X11::XCB

## Status

This is volatile code and the terrain may shift as I (hopefully)
get input from those adventurous enough to use it.  Also,
not ecosystem integrated yet until some reasonable way to graft
the version of the code generator, the version of the XML used,
the version of the supporting static modules, and X11 down-revision
compatibility onto the package Version values.

## Purpose

Bindings to libxcb core and API for using the X11 protocol

This module provides a low-level API to X11 protocol packet
handling, under the X11::XCB:: namespace, and a higher level
API starting in the X11:: namespace.  Both APIs are designed
for asyncronous/multithreaded use.

## High-level API (X11 and X11::various_extension_names)

This is a mostly client-oriented API which offers classes for
common X11 object-like constructs (windows, graphics contexts,
properties, atoms, etc).  It also ties the asyncronous features
of libxcb into the Perl 6 concurrency model.

Most of the documentation for this API will be included
in-source as pod documentation.

## Low-level packet API (X11::XCB and X11::XCB::various_extension_names)

This API is mosly implemented by autogenerated code based
off XmlXcb, a structured definition of the X11 protocol.
The Pod documentation in these files are taken from inline
documentations in these definitions which... could use some
more tuits.  As such a summary of the Perl 6 side of this
API is due:

### Perl 6 packet wrapper objects -- "XCB objects"

For almost all packets in the X11 protocol, a Perl 6 XCB object
is created that maps data in the packet to Any-typed attributes.
This API is meant to be used not just by client programs, but
by programs that intercept, relay, or transform X11 protocol
sessions in a proxy role.

XCB classes are not strict, allowing you to, for example,
annotate certain attributes with values that would not be allowed
in an on-wire packet.  Where a clean mapping to a Perl 6 base
data type is possible, it is employed.  If the object is to
be re-transformed to a wire packet, naturally, you will have
bring the values back into conformance.

A stricter set of XCB classes for type-safety checking is
certainly possible as a future addition.

The following example creates an XCB object for a GetAtomNameReply
packet from packet data.  The :length field can be used to avoid
nasty buffer overruns if something is wrong with the packet.
For now, endianness conversions are assumed to be handled by a
protocol-aware IO layer (a .swab method may be supplied in the
future for that).

```perl6
use X11::XCB::XProto :replies;

my $garp = GetAtomNameReply.new($raw_packet, :length(38));

# ... or you can make a packet up out of thin air:
my $garp2 =  GetAtomNameReply.new(:sequence(4), :name("MyAtom"));

# Most attributes in the object are read-writeable and not strict
# about what types they take.
$garp.name = "foo" but 42;
```

Note there is no .name_length attribute, despite the X11 protocol
having such a packet field -- it is automatically determined
from the encoded string length.

To serialize the data in these objects, a .bufs method is provided.
It will spit out a list of buffers... possibly including some read-only
blobs, and possibly some with different word sizes:

```perl6
use X11::XCB::XProto :replies;
GetAtomNameReply.new(:sequence(4), :name("MyAtom")).bufs.perl.say
# (Buf.new(1,0,4,0,2,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
#  Blob.new(77,121,65,116,111,109), 
#  Blob.new(0,0))
```

In the first buffer in the above example, you can see some magically
appearing values.  The "6" is the encoded length of "MyAtom".
Where did the "1" and the "2" come from?  The "1" is hardcoded for
Reply types as it is the X11 protocol designator for reply
packets.  The "2" is the X11 protocol .length field (40 div 4 - 8).

Each of the XCB classes has a few (read-only) class attributes to
help you access magic values like this if needed.
(TODO more exist and should be made available through methods):

1. Classes that do the roles Request or Reply have .opcode.  For
   core protocol objects this is the major opcode and for extension
   opcodes the minor opcode.

2. Classes that do the role Event have .event_code containing
   the numeric code of the event.  For extension events, this
   is relative to the per-connection event base value.

3. Classes that do the role Error have .error_code containing
   the numeric code of the event.  For extension events, this
   is relative to the per-connection error base value.

### embedded NativeCall packet header classes

Inside each of the XCB classes is a NativeCall class that
maps the static header of a packet (as much as NativeCall can
currently support of it) to C memory layout.  Dynamic content
which may or may not appear in a packet based on previous headers
is not included in the attributes of these classes.  The type
object of this class is available in the XCB object's namespace
under '::cstruct' or through the .cstruct method on the XCB
object.

This NativeCall class supports creating a new instance from the
higher level class through a .nativeize method:

```perl6
use X11::XCB::XProto :replies;
my $n = GetAtomNameReply::cstruct.new;
$n.nativize(GetAtomNameReply.new(:sequence(4), :name("MyAtom")));
$n.perl.say;
# X11::XCB::XProto::GetAtomNameReply::cstruct.new(
#    response_type => 1, sequence => 4, length => 0, name_len => 6,
#    ...and a bunch of padding noise, until NativeCall improves
# );
```

Note the length attribute is not set yet, because the packet is
only a header, not a full packet.  The name itself does not
appear in this object, because it is dynamically sized.

The .Hash method can be used to pull values for those attributes
that exist in its corresponding XCB object out.

```perl6
$n.Hash.say; # only says {sequence => 4}

```

## Namespace and identifier manipulation

The X11 protocol contains many root word collisions which create a
rather hairy namespace problem.  This is why C X11 code often looks
like this:

'''C
return xcb_image_create(width, height, XCB_IMAGE_FORMAT_Z_PIXMAP,
                        fmt->scanline_pad, fmt->depth, fmt->bits_per_pixel,
                        0, setup->image_byte_order, XCB_IMAGE_ORDER_LSB_FIRST,
                        image32, width*height*4, image32);
}
'''

Since Perl 6 has lexically scoped namespace capabilities, this module
leverages this capability to make code neater, while keeping things
close to existing names from both Xlib and XCB for sake of familiarity.

### enums, in general

Each enum in the API is wrapped in a class to prevent it from spilling
into an importer's namespace by default.  The class names of these
classes are the basename of the enumerator followed by 'Enum'.  Thus
it is safe to export almost all of the wrapping classes by default.
A handful of identically named enums in different extensions have been
worked around by not exporting them by default.  These are:

 * XProto::NotifyModeEnum
 * XProto::NotifyDetailEnum
 * XInput::NotifyModeEnum
 * XInput::NotifyDetailEnum

These can still be imported by specifying the ":internal" export tag,
if you are sure no conflicts will result.

So, if you value keeping your namespace relatively clean, you can use
enums straight up by specifying their wrapping class everywhere:

```perl6
use X11::XCB::XProto;
ImageOrderEnum::LSBFirst.perl.say; # says ImageOrder::LSBFirst
say +ImageOrderEnum::LSBFirst;     # says 0
}
```

...further, within most of these classes the enum itself has been
marked to export using the ":enums" tag.  So if you want to load
the individual symbols from a single enum directly into a namespace,
you may import them (and along with them the name of the actual enum):

```perl6
use X11::XCB::XProto;
import ImageOrderEnum :enums;
LSBFirst.perl.say;                 # says ImageOrder::LSBFirst
say +LSBFirst;                     # says 0
```

In some cases some a tag other than ":enum" is used for
special subgroups of enums.  These cases mostly make sense
from a use case perspective, but to tell the truth they
are mostly making lemonade out of the fact that exported
symbols are unit scoped rather than scoped to their
containing class, so two enums cannot have the same symbol
if they can be exported. (Currently due to RT#127305, there
are also extra hoops to jump through.)

Also, in any case where enums use identifiers that conflict with
Perl 6 core base names, and munging their names would be messy,
the tag from which they may be imported will include the text
"danger" -- and you should probably take care to lexically
contain them.

To see which enums do not use the enum tag, and what tag they
use, examine %EnumValueExports from the X11::XCBquirks module.

Note that any enum values using the rather common names
C<None>, C<Success>, and C<Normal> will be renamed, but since the
vast majority of occurrences of these identifiers have the same
value, X11::XCB::XProto exports constants which you can use
in their place.  Or just use 0.  But verify the enum you want
is not one of the exceptions that has a different value, first.

### enums in value_lists/value_masks

The X11 protocol sometimes uses a bitfield to indicate the
presence of optional packet fields.  In a Perl 6 XCB object,
this finagling is hidden behind a hash attribute.  The keys to
this hash are of an appropriate enum type.  So, to fill a value-pair
in a perl6 XCB object you only ned to set the values:

```perl6
my $cwrq = ConfigureWindowRequest.new(
            :window($.wid.value),
            :value_list(CWHeight, self.height)
        );
```

...note that due to enums not quite getting along with fat-arrows,
you should not use them (just use commas), because they will
stringify instead of becoming integer values:

```perl6
my $cwrq = ConfigureWindowRequest.new(
            :window($.wid.value),
            :value_list(CWHeight => self.height)
        );
# ... will produce an error message something like:
# Type check failed in binding to key;
# expected ConfigWindow but got Str ("CWHeight")
```

Currently you must use the enums.  Hopefully in future versions of
Perl 6 you'll be able use the integer values directly if you really
want to... though the higher level API will probably help you avoid
using value_lists altogether.

### enums that select between classes

Certain enums in the XCB API are meant to map integer values to
classes, e.g. when those classes are part of a union, or when payload
of a packet can have various related classes.  In this case, instead of
an enum, these classes are taught to numify to those integers.
Only the type objects numify.  So to get that numeric value, simply
use the class name:

```perl6
use X11::XCB::RandR;
say +ProviderChange # says 2
```
...A subroutine is also provided to translate integers to the appropriate
class.  With no arguments it outputs all the entries in the map as
pairs:

```perl6
use X11::XCB::RandR;
&NotifyData(2).name.say; # says ProviderChange
&NotifyData().elems.say; # says 6
```

...unfortunately this no longer works without using the C<&> sigil.

A parameteric role by the same name of the subroutine is mixed into the
member classes.  Also inside the namespace of that role, ::cstruct
refers to a CUnion structure composed of all the respective ::cstruct
of the member classes.

Classes belonging to such a group can be autodetected by the
constructors of classes that contain them, or by that of the
role itself in the case of a tagged union, so there is no
need for separate handling of the type discriminator field.
