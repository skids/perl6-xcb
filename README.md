perl6-xcb
========

X11::XCB

## Purpose

Bindings to libxcb core and API for using the X11 protocol

Don't use this yet.  Things WILL be changing drastically.  I just
had sunk enough time into it I felt I needed to back it up,
and since It'll be published here anyway, what better place.


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

Since Perl6 has lexically scoped namespace capabilities, this module
leverages this capability to make code neater.

### enums

Each enum in the API is wrapped in a class to prevent it from spilling
into an importer's namespace by default.  The class names of these
classes are the basename of the enumerator followed by 'Enum'.  Thus
it is safe to export almost all of the wrapping classes by default.
A handful of identically named enums in different extensions have been
worked around by not exporting them by default.  All such classes
are available by specifying the ":internal" export tag, if you are
sure no conflicts will result.

So if you value keeping your namespace relatively clean, you can use
enums straight up by specifying their wrapping class:

```perl6
use X11::XCB::XProto;
ImageOrderEnum::LSBFirst.perl.say; # says ImageOrder::LSBFirst
say +ImageOrderEnum::LSBFirst;     # says 0
}
```

...further, within each of these classes the enum itself has been
marked to export using the ":enums" tag.  So if you want to load
the individual symbols from a single enum directly into a namespace,
you may import them:

```perl6
use X11::XCB::XProto;
import ImageOrderEnum :enums;
LSBFirst.perl.say;                 # says ImageOrder::LSBFirst
say +LSBFirst;                     # says 0
```

...There are a few enums that use identifiers that conflict with
Perl6 core base names.  In order to import these, you must use the
":danger" tag -- and you should probably take care to lexically
contain them.

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
NotifyData(2).name.say; # says ProviderChange
NotifyData().elems.say; # says 6
```

A parameteric role by the same name of the subroutine is mixed into the
member classes.  Also inside the namespace of that role, ::cstruct
refers to a CUnion structure composed of all the respective ::cstruct
of the member classes.
