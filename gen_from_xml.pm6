#!/usr/bin/env perl6
use XML;

# Places we are likely to find XCB-XML files
my @xmlpaths = "/usr/share/xcb";

class param is rw {
    has $.name;
    has $.c_attr = |();
    has $.p_attr = |();
    has $.c_doc = |();
    has $.p_doc = |();
    has $.c2p_arg = |();
    has $.c2p_code = |();
    has $.p2c_init = |();
    has $.p2c_code = |();
}

# Need a light-duty ordered, autovivifying typed Associative?  Sinch.
class params does Associative {
    has param @.params;
    method AT-KEY (Str $key) is rw {
my $res = @.params.first(*.name eq $key);
unless $res.defined {
    @.params.push(param.new(:name($key)));
    $res = @.params[*-1];
}
$res;
    }
    method EXISTS-KEY ($key) is rw {
        so @.params.first(*.name eq $key);
    }
    method DELETE-KEY ($key) is rw {
        my @res = @.params.grep(*.name eq $key);
        @.params .= grep(*.name ne $key);
        @res;
    }
}

class mod {
    has $.xml;
    has $.cname;
    has $.outname;
    has $.modname;
    has $.extension;
    has $.xextension;
    has $.prologue is rw;
    has @.enums is rw;
    has @.typedefs is rw;
    has @.cstructs is rw;
    has @.p6classes is rw;
    has %.opcodes is rw;
    has %.errors is rw;
}

# Keep track of XML coverage during devel.
my $TODOP6 = "TODOP6_0";

sub MAIN (:$xmldir? is copy) {
    unless $xmldir {
        $xmldir = @xmlpaths.grep(*.IO.d);
    }
    unless $xmldir {
        die q:to<EOHELP>;
            Could not find directory with XCB XML files.
            Please specify it as an option e.g. :xmldir(/path/to/xml)
            EOHELP
    }
    $xmldir .= IO;
    my @xmlfiles = $xmldir.dir.grep(*.extension eq any <XML xml>);

    # TODOP6: Skipping xkb and xinput for now.  Enums need a lot of work.
    @xmlfiles = @xmlfiles.grep(*.basename ne any <xkb.xml>);

# for fast testing
#@xmlfiles = @xmlfiles.grep(*.basename eq any <xproto.xml res.xml>);
    my @mods;
    @mods.push(MakeMod($_)) for @xmlfiles;
    MakeImports($_, @mods) for @mods;
    GetOpcodes($_) for @mods;
    MakeEnums($_) for @mods;
    MakeTypeDefs($_) for @mods;
    MakeErrors($_) for @mods;
    MakeErrors2($_) for @mods;
    MakeStructs($_) for @mods;
    MakeReplies($_) for @mods;
    MakeRequests($_) for @mods;
#.enums.say for @mods;
    Output($_) for @mods;
}

sub MakeMod ($xml) {
    my $xmltree = from-xml-file($xml.Str);
    unless $xmltree.root.name eq "xcb" {
        die "file {$xml.Str} root tag of xcb not found"
    }
    my $outname;
    my $cname;
    my $modname;
    my $extension;
    my $xextension;
    given $xmltree.root {
        $cname = $modname = .attribs<header>;
        if $modname eq "xproto" {
            $modname = "XProto";
            $extension = False;
        }
        else {
            $extension = .attribs<extension-name>;
            $xextension = .attribs<extension-xname>;
            $modname = $extension;
        }
        $outname = "$modname.pm6";
    }

    my $prologue = qq:to<EOP>;
        unit module X11::XCB::$modname;

        use NativeCall;
        use X11::XCB :internal :DEFAULT;
        EOP

    if $extension {
        $prologue ~= qq:to<EOE>;

            our \$xcb_{$cname}_id is export(:internal) :=
                cglobal("{$cname}", "xcb_{$cname}_id", xcb_extension_t);

            EOE
    } else {
        $prologue ~= qq:to<EOE>;

            # Provide a way to cheese around lots of enums using these
            constant None is export(:internal :enums) = 0;
            constant Success is export(:internal :enums) = 0;

            EOE
    }

    mod.new(:xml($xmltree), :$outname, :$modname, :$extension
            :$xextension, :$cname, :$prologue);
}

our %nctypemap = ();
sub NCtype ($t is copy) {
    my $o = $t;
    $t = %nctypemap{$t} while %nctypemap{$t}:exists;
    $t ~~ s/^CARD/uint/;
    $t ~~ s/^BOOL$/uint8/;
    $t ~~ s/^BYTE$/uint8/;
    $t ~~ s/^INT/int/;
    $t ~~ s/^char$/uint8/;
    $t ~~ s/^float$/num32/;
    $t ~~ s/^double$/num64/;
    %nctypemap{$o} = $t unless $o eq $t;
    ($t eq any <int8 int16 int32 int64
               uint8 uint16 uint32 uint64
               long longlong num32 num64
               Str CArray[int32] Pointer[void]
               bool size_t>) ?? $t !! "TODOP6";
}

sub MakeTypeDefs ($mod) {
    for (|$mod.xml.root.elements(:TAG<xidtype>),|$mod.xml.root.elements(:TAG<xidunion>)) -> $e {
        my $t = $e.attribs<name>;
        $t = "Glx$t" if $mod.cname eq "glx";
        $t = %nctypemap{$t} = $t ~ "ID";
        %nctypemap{$t} = "uint32";
        $mod.typedefs.push: "constant $t" ~
            " is export(:internal :ctypes) = uint32;\n";
    }
    for $mod.xml.root.elements(:TAG<typedef>) -> $e {
        %nctypemap{$e.attribs<newname>} = $e.attribs<oldname>;
        my $t = NCtype($e.attribs<oldname>);
        $mod.typedefs.push: "constant {$e.attribs<newname>}" ~
            " is export(:internal :ctypes) = $t;\n";
    }
}

our %EnumRemap;
sub MakeEnums ($mod) {

    # Fixup/perlify for enum type names that conflict
    my sub fix_name($enum) {
        return $mod.modname ~ $enum
            if $mod.cname eq "present" and $enum eq any <Event EventMask>
            or $mod.cname eq "randr"   and $enum eq any <Connection>
            or $mod.cname eq "glx"     and $enum eq any <GC>
            or $enum eq any <Control Cursor>;

        return "ScreenSaverState"
            if $enum eq "ScreenSaver";

        return $enum;
    }

    # Keep some things in their own namespace rather than munging them
    my sub fix_export($enum) {
        my @elements = $enum.elements(:TAG<item>);
        my @res = " is export(:DEFAULT :internal)", " is export(:enums)";

        # Perl6 things that can be overidden, but avoid surprises
        # TODOP6: will have to go look for more of these pre-publication
        if @elements.first(*.attribs<name> eq any <
             Cursor
            >) {
            @res[1] = " is export(:danger)";
        }

        # conflicts within or between modules
        @res[0] = " is export(:internal)" if $enum.attribs<name> eq any <
            NotifyDetail NotifyMode
        >;

        |@res;
    }

    # Fixup/perlify for enum value names that conflict
    my sub fix_valname($item, $from) {
        return "$from$item" if $item ~~ /^\d+/;

        # A bunch of masks named the same as their values
        return $item ~ "Mask" if $from
            eq any <PresentEventMask SelectionEventMask
                    CursorNotifyMask NotifyMask>;

        return "CW$item"      if $from eq any <CW ConfigWindow>;
        return "SaveSet$item" if $from ~~ /^SaveSet/;
        return "Alarm$item"   if $from eq "ALARMSTATE";
        return "Video$item"   if $from eq "VideoNotifyReason";

        return "$from$item"
            if $mod.cname eq "render" and $from eq any <Repeat CP>
            or $mod.cname eq "xv" and $from eq any <GrabPortStatus>
            or $mod.cname eq "xkb" and $from eq any <Groups GroupsWrap>
            or $mod.cname eq "glx" and $from eq any <PBCDT>
            or $mod.cname eq "present"
                and $from eq any <Option Capability CompleteMode CompleteKind>
            or $mod.cname eq "randr"
                and $from eq any <SetConfig RandRConnection SelectInput>
            or $from eq any <
                GrabMode LineStyle FillStyle CapStyle JoinStyle GC
            >
            # Individual values
            or $item eq "PointerRoot" and $from eq "InputFocus"
            or $item eq any <
                None Success Off On Any Default Normal
                Lock Shift Insert Delete Button
                Control Pointer Cursor Async
                Grab LedMode AutoRepeatMode
            >;

        return "VISUAL"       if $item eq "VISUALID"; # restore consistency

        return $item;
    }

    for $mod.xml.root.elements(:TAG<enum>) -> $e {

        my $ename = fix_name($e.attribs<name>);
        %EnumRemap{"{$mod.cname} {$e.attribs<name>.Str}"} = $ename;
        (my $export_ext, my $export_int) = fix_export($e);

        $mod.enums.push:
        "our class {$ename}Enum$export_ext \{\n" ~
        "    our enum {$ename}$export_int «\n        " ~
        (for $e.elements(:TAG<item>) -> $item {
            state $lastval = -Inf;
            my $v = $item.elements[0];
            my $n = fix_valname($item.attribs<name>, $ename);
            if $v.name eq "bit" {
                $v = 1 +< $v.nodes[0].text;
            }
            else {
                $v = $v.nodes[0].text;
            }
            if $v == $lastval + 1 {
                $lastval = $v; "$n"
            }
            else {
                ":$n\({$lastval = $v})"
            }
        }).join("\n        ") ~ "\n»;\n}\n\n";
    }
}

sub MakeImports($for, @mods) {
    my @imports = $for.xml.root.elements(:TAG<import>);
    my $res = "";
    for @imports -> $cname {
        my $from = @mods.first(*.cname eq $cname.contents);
        $res ~= "use X11::XCB::$from.modname() :internal :DEFAULT;\n";
    }
    $for.prologue ~= $res;
}

multi sub build_op($f where {.name eq "op"}) {
    return "TODOP6 XXX" if $f.elements.elems != 2;
    my $p6op = do given $f.attribs<op> {
        when '+'|'-'|'*'|'|' { $_ }
        when '&' { '+&' }
        when '/' { 'div' }
        default { "TODOP6 $_" }
    }
    "(" ~ build_op($f.elements[0]) ~ " $p6op "
        ~ build_op($f.elements[1]) ~ ")";
}

multi sub build_op($f where {.name eq "unop"}) {
    return "TODOP6 XXX" if $f.elements.elems != 1;
    my $p6op = do given $f.attribs<op> {
        when '~' { '+^' }
        default { "TODOP6 $_" }
    }
    "($p6op" ~ build_op($f.elements[0]) ~ ")";
}

multi sub build_op($f where {.name eq "fieldref"}) {
    '$pstruct.' ~ $f.contents.Str;
}

multi sub build_op($f where {.name eq "value"}) {
    $f.contents.Str;
}

sub build_equation($f) {
   return "Not one root op" if $f.elements».name.join(" ") ne any <op unop>;
   build_op($f.elements[0]);
}

my %cstructs;
sub MakeCStructField(params $p, $f, $padnum is rw, $found_list is rw, $rw = " is rw") {
    given $f.name {
        when "field"|"exprfield" {
            use NativeCall;
            my $name = $f.attribs<name>;
            my $type = $f.attribs<type>;
            my $has = "has";
            my $expr = $f.name eq "exprfield"
                     ?? "\n#{++$TODOP6} exprfield ^^" !! "";

            $p{$name}.c_attr = $found_list
                ?? "# {NCtype($type)} \$.$name $expr unfixed offset"
                !! "has {NCtype($type)} \$.$name$rw;$expr";

            $p{$name}.p_attr = "has \$.$name is rw;$expr";

            if $found_list {
                $p{$name}.p2c_code = qq:to<EOPC>;
                    \{
                        my \$cl = class :: is repr("CStruct") \{
                           has {NCtype($type)} \$.foo;
                        }
                        my \$ca = nativecast(CArray[uint8],
                                            \$cl.new(:foo(\$.$name)));
                        @bufs.push:
                            Blob.new(\$ca[^nativesizeof({NCtype($type)})])
                    }
                    EOPC
            }
            else {
                $p{$name}.c2p_arg = ":{$name}(\$\!$name)";
                $p{$name}.p2c_init = "\$\!{$name} = \$p6.{$name};$expr";
            }

            if %cstructs{$type}:exists {
                my $pptype = %cstructs{$type};
                if $found_list {
                    $p{$name}.c2p_code = qq:to<EOPC>;
                        @args.append:
                            "$name",
                            $pptype\.new(Pointer.new(\$p + \$oleft - \$left),
                                         :\$left, :!free);
                        EOPC
                    $p{$name}.p2c_init = qq:to<EOPI>;
                        \{
                            my \$c = {$pptype}::cstruct.nativeize(\$p6.$name);
                            nativecast(CArray[uint8],\$\!$name)[
                                ^nativesizeof({$pptype}::cstruct)] =
                                nativecast(CArray[uint8],\$c)[
                                    ^nativesizeof({$pptype}::cstruct)];
                        }
                        EOPI
                }
                else {
                    $p{$name}.c_attr = qq:to<EOCT>;
                        HAS {$pptype}::cstruct \$.$name$rw;$expr;
                        EOCT
                    $p{$name}.c2p_arg = qq:to<EOPA>;
                        :{$name}({$pptype}.new(nativecast(Pointer[uint8],\$\!$name),
                                   :left(nativesizeof({$pptype}::cstruct)), :!free))
                        EOPA
                }
            }
            else {
                $p{$name}.c_attr ~= qq:to<EOCA>;

                constant {$name}___maxof =
                    2 ** (nativesizeof({NCtype($type)}) * 8) - 1;
                EOCA
                if $found_list {
                    $p{$name}.c2p_code = qq:to<EOPC>;
                        @args.append:
                            "$name",
                            (if \$left >= nativesizeof({NCtype($type)}) \{
                                LEAVE \{ \$left -= nativesizeof({NCtype($type)}); }
                                # Strange incantation necessary
                                nativecast(Pointer[{NCtype($type)}], Pointer.new(\$p + \$oleft - \$left)).deref
                             }
                             else \{ die "Short Packet" }
                            )
                        EOPC
                }
            }
        }
        when "pad" {
            if $f.attribs<align>:exists {
                ++$TODOP6;
                $p{$TODOP6}.c_attr = "# $TODOP6: alignpad";
                $p{$TODOP6}.p_attr = "# $TODOP6: alignpad";
                succeed;
            }
            if $found_list {
                ++$TODOP6;
                $p{$TODOP6}.c_attr = "# $TODOP6: pad between dynamic lists";
                $p{$TODOP6}.p_attr = "# $TODOP6: pad between dynamic lists";
                succeed;
            }
            $padnum++;
            (for 0..^$f.attribs<bytes> {
                given "pad{$padnum}_$_" {
                   $p{$_}.c_attr = "has uint8 \$.$_;";
                   $p{$_}.p_attr = "# padding here in CStruct";
                }
            }).join(" ");
        }
        when "list" {
            $found_list++;
            if $f.attribs<name> ~~ /^alignment_pad/ {
                my $align = 4;
                # Map String directly to Perl6 Str
                my $name = $f.attribs<name>;
                $p{$name}.c_attr = "# Dynamic layout: alignment padding";
                $p{$name}.p_attr = "# Padding for alignment here in CStruct";
                $TODOP6++;
                $p{$name}.p2c_code = qq:to<EOCC>;
                    # $TODOP6 need to align here
                    EOCC
                $p{$name}.c2p_code = qq:to<EOPC>;
                    if ($align) \{
                        my \$newp = Pointer[uint8].new(\$p);
                        my \$oldp = nativecast(Pointer[uint8], \$pstruct);

                        \$left -= (\$newp - \$oldp) % $align;
                        die("Short packet")
                            unless \$left >= 0;
                    }
                    EOPC
            }
            elsif +$f.elements and
                $f.elements».name.join(" ") ne any <fieldref value> {
                my $eq = build_equation($f);
                if ($eq ~~ /TODOP6/) {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 complicated $_ ($eq) of {$f.attribs<type>}";
                    $p{$TODOP6}.p_attr = "# $TODOP6 complicated $_ ($eq) of {$f.attribs<type>}";
                }
                elsif $f.attribs<type> eq any <char STRING8> {

                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;

                    if $eq ~~ /pstruct\.<!before length<!alpha>>/ {
                        $TODOP6++;
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s $TODOP6 fields other than .length";
                    } else {
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    }
                    $p{$name}.p_attr = "has \$.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        given \$\.{$name}.encode('utf8') \{
                            if .elems \{ \@bufs.push(Blob.new(\$_.values)) }
                        }
                        EOCC
                    $p{$name}.c2p_code = qq:to<EOPC>;
                        die("Short packet")
                            unless \$left >= $eq;
                        @args.append: "$name",
                            (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[^$eq]
                            ).decode("utf8"));
                        \$left -= $eq;
                        EOPC
                }
                elsif %cstructs{$f.attribs<type>} -> $pt {
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;

                    if $eq ~~ /pstruct\.<!before length<!alpha>>/ {
                        $TODOP6++;
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s $TODOP6 fields other than .length";
                    } else {
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    }
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push(\$_.bufs) for \$.$name;
                        EOCC
                    $p{$name}.c2p_code = qq:to<EOPC>;
                    @args.append:
                        "$name",
                        (for 0..^$eq \{
                                # XXX no assurance pointer math will not start adding by sizeof
                                $pt\.new(Pointer.new(\$p + \$oleft - \$left),
                                         :\$left, :!free);
                            }
                        );
                    EOPC
                }
                else {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 NYI $_";
                    $p{$TODOP6}.p_attr = "# $TODOP6 NYI $_";
                }
            }
            elsif $f.elements(:TAG<value>) -> [ $val ] {
                my $frval = $val.contents.Str.Int;
                if $f.attribs<type> eq "char" {
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;
                    $p{$name}.c_attr = "# Dynamic layout: $name\[$val] chars";
                    $p{$name}.p_attr = "has \$.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                    given \$\.{$name}.encode('utf8') \{
                        if .elems \{ \@bufs.push(Blob.new(\$_.values)) }
                    }
                    EOCC
                    $p{$name}.c2p_code = qq:to<EOPC>;
                    die("Short packet")
                        unless \$left >= $frval;
                    @args.append: "$name",
                        (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[^$frval]
                        ).decode("utf8"));
                    \$left -= $frval;
                    EOPC
                }
                elsif NCtype($f.attribs<type>) eq any <
                    int8 int16 int32 int64 uint8 uint16 uint32 uint64 long
                    longlong bool size_t
                > {
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;
                    my $nct = NCtype($type);

                    $p{$name}.c_attr =
                        "# Dynamic layout: $name\[$val] of $type";
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push: Blob[$nct].new(|\@.$name);
                        EOCC
                }
                else {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 nonchar $_";
                    $p{$TODOP6}.p_attr = "# $TODOP6 nonchar $_";
                }
            }
            elsif $f.elements(:TAG<fieldref>) -> [ $fr ] {
                my $frname = $fr.contents.Str;
                if $f.attribs<type> eq any <char STRING8> {
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;

                    # Go back and remove the _len field from perl6 attributes
                    $p{$frname}.p_attr = "# $frname not needed in P6 object";

                    $p{$frname}.p2c_init = qq:to<EOPC>;
                        my \${$frname}___sizeof = \$p6.{$name}.encode('utf8').bytes;
                        die ("Maximum field size exceeded")
                            if \${$frname}___sizeof > {$frname}___maxof;
                        EOPC

                    my $c2p_len = $p{$frname}.c2p_code;
                    if $c2p_len ~~ s/^.*?\"$frname\"\s*\,// {
                        $p{$frname}.c2p_code = qq:to<EOC1>;
                            # No p6 attribute $frname to init but may need value in place
                            my \${$frname}___inplace = $c2p_len;
                            EOC1
                    }
                    else {
                        $c2p_len = "";
                        $p{$frname}.c2p_code = "# No p6 attribute $frname to init";
                    }
                    if $p{$frname}.c_attr ~~ /^\#/ {
                       my $lc = "self.{$name}.encode('utf8').bytes";
                       if $p{$frname}.p2c_code ~~
                           /\$\.$frname <!before <alpha>|\d>/ {
                           $p{$frname}.p2c_code ~~
                               s:g/\$\.$frname <!before <alpha>|\d>/$lc/;
                       }
                    }
                    else {
                        $p{$frname}.p2c_init ~=
                            "\n\$\!$frname = \${$frname}___sizeof;"
                    }

                    $p{$frname}.c2p_arg = |();
                    $p{$name}.c_attr = "# Dynamic layout: chars";
                    $p{$name}.p_attr = "has \$.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        given \$\.{$name}.encode('utf8') \{
                            # XXX could we just pass the utf8 here?
                            if .elems \{ \@bufs.push(Blob.new(\$_.values)) }
                        }
                        EOCC

                    $p{$name}.c2p_code = $c2p_len ?? qq:to<EOP1> !! qq:to<EOP2>;
                            die("Short packet")
                                unless \$left >= \${$frname}___inplace;
                            @args.append:
                                "$name",
                                (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[
                                    ^\${$frname}___inplace
                                ]).decode("utf8"));
                            \$left -= \${$frname}___inplace;
                            EOP1
                            die("Short packet")
                                unless \$left >= \$pstruct\.$frname;
                            @args.append:
                                "$name",
                                (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[
                                    ^\$pstruct\.$frname
                                ]).decode("utf8"));
                            \$left -= \$pstruct\.$frname;
                            EOP2
                }
                elsif NCtype($f.attribs<type>) eq any <
                    int8 int16 int32 int64 uint8 uint16 uint32 uint64 long
                    longlong bool size_t
                > {
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;
                    my $nct = NCtype($type);

                    # Go back and remove the _len field from perl6 attributes
                    $p{$frname}.p_attr = "# $frname not needed in P6 object";
                    $p{$frname}.p2c_init = qq:to<EOPI>;
                        die ("Maximum field size exceeded")
                            if \$p6.{$name}.elems > {$frname}___maxof;
                        EOPI
                    my $c2p_len = $p{$frname}.c2p_code;
                    if $c2p_len ~~ s/^.*?\"$frname\"\s*\,// {
                        $p{$frname}.c2p_code = qq:to<EOC1>;
                            # No p6 attribute $frname to init but may need value in place
                            my \${$frname}___inplace = $c2p_len;
                            EOC1
                    }
                    else {
                        $c2p_len = "";
                        $p{$frname}.c2p_code = "# No p6 attribute $frname to init";
                    }
                    if $p{$frname}.c_attr ~~ /^\#/ {
                       my $lc = "self.{$name}.elems";
                       if $p{$frname}.p2c_code ~~
                           /\$\.$frname <!before <alpha>|\d>/ {
                           $p{$frname}.p2c_code ~~
                               s:g/\$\.$frname <!before <alpha>|\d>/$lc/;
                       }
                    }
                    else {
                       $p{$frname}.p2c_init ~= "\n\$\!$frname = \$p6.{$name}.elems;"
                    }
                    $p{$frname}.c2p_arg = |();
                    $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push: Blob[$nct].new(|\@.$name);
                        EOCC
                    $p{$name}.c2p_code = $c2p_len ?? qq:to<EOP1> !! qq:to<EOP2>;
                        @args.append:
                            "$name",
                            (for 0..^\${$frname}___inplace \{
                                    # XXX no assurance pointer math will not start adding by sizeof
                                die "Short Packet" unless \$left >= nativesizeof($nct);
                                NEXT \{ \$left -= nativesizeof($nct) };
                                nativecast(Pointer[{$nct}],Pointer.new(\$p + \$oleft - \$left)).deref;
                            });
                        EOP1
                        @args.append:
                            "$name",
                            (for 0..^\$pstruct.$frname \{
                                    # XXX no assurance pointer math will not start adding by sizeof
                                die "Short Packet" unless \$left >= nativesizeof($nct);
                                NEXT \{ \$left -= nativesizeof($nct) };
                                nativecast(Pointer[{$nct}],Pointer.new(\$p + \$oleft - \$left)).deref;
                            });
                        EOP2
                }
                elsif $f.attribs<type> eq "STR" {
                    # Map String directly to Perl6 Str
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;
                    # Go back and remove the _len field from perl6 attributes
                    $p{$frname}.p_attr = "# $frname not needed in P6 object";

                    $p{$frname}.p2c_init = qq:to<EOPC>;
                        my \${$frname}___sizeof = \$p6.{$name}.elems;
                        die ("Maximum field size exceeded")
                            if \${$frname}___sizeof > {$frname}___maxof;
                        \$\!$frname = \${$frname}___sizeof;
                        EOPC
                    $p{$frname}.c2p_arg = |();
                    $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push(String.new(:name(\$_)).bufs) for \$.$name;
                        EOCC
                    $p{$name}.c2p_code = qq:to<EOPC>;
                    @args.append:
                        "$name",
                        (for 0..^\$pstruct.$frname \{
                                # XXX no assurance pointer math will not start adding by sizeof
                                String.new(Pointer.new(\$p + \$oleft - \$left),
                                           :\$left, :!free).name;
                            }
                        );
                    EOPC
                }
                elsif %cstructs{$f.attribs<type>} -> $pt {
                    my $name = $f.attribs<name>;
                    my $type = $f.attribs<type>;
                    # Go back and remove the _len field from perl6 attributes
                    $p{$frname}.p_attr = "# $frname not needed in P6 object";

                    $p{$frname}.p2c_init = qq:to<EOPC>;
                        my \${$frname}___sizeof = \$p6.{$name}.elems;
                        die ("Maximum field size exceeded")
                            if \${$frname}___sizeof > {$frname}___maxof;
                        \$\!$frname = \${$frname}___sizeof;
                        EOPC
                    $p{$frname}.c2p_arg = |();
                    $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push(\$_.bufs) for \$.$name;
                        EOCC
                    $p{$name}.c2p_code = qq:to<EOPC>;
                    @args.append:
                        "$name",
                        (for 0..^\$pstruct.$frname \{
                                # XXX no assurance pointer math will not start adding by sizeof
                                $pt\.new(Pointer.new(\$p + \$oleft - \$left),
                                         :\$left, :!free);
                            }
                        );
                    EOPC
                }
                else {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 nonchar $_";
                    $p{$TODOP6}.p_attr = "# $TODOP6 nonchar $_";
                }
            }
        }
        when "valueparam" {
            # Replaced by "switch" but some people may be working
            # off older xml files.

            # One of the deficiencies it had was not listing the
            # enum of the mask bit values.  So we fix this up.
            my %vpmap = (
                :CreateWindow<CW>
            );
            my $parent = $f.parent;
            $parent = $parent.parent if $parent.name eq "reply";
            $parent = $parent.attribs<name>;
            unless (%vpmap{$parent}:exists) {
                $TODOP6++;
                $p{$TODOP6}.c_attr = "# $TODOP6 unmapped valueparam";
                $p{$TODOP6}.p_attr = "# $TODOP6 unmapped valueparam";
                succeed;
            }
            my $enum = %vpmap{$parent};
            # TODO if anything uses enums from an include;
            my $renum = %EnumRemap.pairs.first({$_.key ~~ /.*\s$enum/});
            my $type = $f.attribs<value-mask-type>;
            my $name = $f.attribs<value-mask-name>;
            my $pname = $f.attribs<value-list-name>;
            $p{$name}.c_attr = qq:to<EOCT>;
                has {NCtype($type)} \$.$name is rw;
                constant {$name}___maxof =
                    2 ** (nativesizeof({NCtype($type)}) * 8) - 1;
                # Dynamic layout -- bit enabled fields
                EOCT
            $p{$name}.p_attr = qq:to<EOPT>;
                # Perl6 object does not need attribute for $name
                constant {$name}___maxof =
                    2 ** (nativesizeof({NCtype($type)}) * 8) - 1;
                has \%.$pname is rw;
                EOPT
            $p{$name}.p2c_code = qq:to<EOPC>;
                \{
                    my \$b;
                    loop (\$b = 1; \$b < {$name}___maxof; \$b +<= 1) \{
                        last if \%.$pname\{\$b}:exists;
                    }
                    if \$b < {$name}___maxof \{
                        @bufs.push: Buf[uint32].new(
                            (loop (\$b = 1; \$b < {$name}___maxof; \$b +<= 1) \{
                                if \%.$pname\{\$b}:exists \{                            
                                    (+\%.$pname\{\$b}) +& 0xffffffff;
                                }
                             }))
                    }
                }
                EOPC
            $p{$name}.p2c_init = qq:to<EOPI>;
                \$\!{$name} = 0;
                \{
                    my \$b = 1;
                    while \$b < {$name}___maxof \{
                            \$\!{$name} +|= \$b if \$p6.$pname\{\$b}:exists;
                            \$b +<= 1;
                    };
                }
                EOPI

        }
        when "doc" | "reply" {
            succeed;
        }
        default {
            $TODOP6++;
            $p{$TODOP6}.c_attr = "# $TODOP6 $_";
            $p{$TODOP6}.p_attr = "# $TODOP6 $_";
        }
    }
}

sub GetOpcodes($mod) {
    for $mod.xml.root.elements(:TAG<request>) -> $req {
        my $name = $req.attribs<name>;
        $name = $mod.modname ~ $name
            if $mod.extension and $name eq "QueryVersion"
            or $mod.extension and $name eq "CreateCursor"
            or $mod.extension and $name eq "DestroyContext"
            or $mod.extension and $name eq "GetVersion"
            or $mod.extension and $name eq "QueryBestSize"
            or $mod.cname eq "present"; # has really conflicty names
        $mod.opcodes{$req.attribs<opcode>} = $name;
    }
}

sub MakeClassDocs($xml, $clname) {
    my @doc;
    if $xml.elements(:TAG<doc>) -> [ $doc ] {
        if $doc.elements(:TAG<brief>) -> [ $brief ] {
            @doc.push("#| $clname -- {$brief.contents}");
            @doc.push("#|");
        }
        if $doc.elements(:TAG<description>) or $doc.elements(:TAG<example>) {
            @doc.push("#| (Docs generated from XCB-XML, " ~
                      "may contain C language specific material)");
            @doc.push("#|");
        }
        if $doc.elements(:TAG<description>) -> [ $desc ] {

            @doc.push("#| $_") for $desc.cdata».data».Str.lines;
            @doc.push("#|");
        }
        if $doc.elements(:TAG<example>) -> [ $ex ] {
            @doc.push("#| C EXAMPLE:");
            @doc.push("#|    ");
            @doc.push("#|    $_") for $ex.cdata».data».Str.lines;
            @doc.push("#|    ");
        }
        if $doc.elements(:TAG<error>) -> @errs {
            @doc.push("#| ERRORS:");
            @doc.push("#|    ");
            for @errs -> $err {
                @doc.push("#| {$err.attribs<type>} -- ");
                @doc.push("#|    $_") for $err.cdata».data».Str.lines;
            }
        }
        if $doc.elements(:TAG<see>) -> @xref {
            @doc.push("#| SEE ALSO: " ~
                      (.attribs<name> for @xref).join(", "));
        }
        if $doc.elements.grep(*.name ne any <
                              description brief example error see field>)
            -> @todos {
            @doc.push("#| TODOP6 unknown doc fields {@todos».name}");
        }
    }
    @doc
}

my %errorcopies;
sub MakeErrors($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    for $mod.xml.root.elements(:TAG<error>) -> $error {
        my params $p .= new;
        my $padnum = -1;
        my $found_list = 0;

        %errorcopies{$error.attribs<name>.Str} := $p;
        for $error.elements -> $e {
            MakeCStructField($p, $e, $padnum, $found_list);
        }
        for $error.elements -> $e {
            if $error.elements(:TAG<doc>) -> [ $doc ] {
                 if $doc.elements(:TAG<field>).grep(
                     {$e.attribs<name>:exists and 
                      $_.attribs<name> eq $e.attribs<name>}) -> [ $fdoc ] {
                     my $docstr =
                         ("#| $_" for $fdoc.cdata».data».Str.lines).join("\n");
                     $p{$e.attribs<name>}.p_doc = $docstr
                            if $p{$e.attribs<name>}.p_attr !~~ /^\#/;
                     $p{$e.attribs<name>}.c_doc = $docstr;
                 }
            }
        }
    }
}

sub MakeErrors2($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    for (|$mod.xml.root.elements(:TAG<error>),|$mod.xml.root.elements(:TAG<errorcopy>)) -> $error {
        my params $p;
        my $padnum = -1;

        if $error.name eq "errorcopy" {
            $p := %errorcopies{$error.attribs<ref>.Str}
        }
        else {
            $p := %errorcopies{$error.attribs<name>.Str};
        }
        my $number = $error.attribs<number>;

        @cstructs.push(qq:to<EOCS>);

                our class cstruct is repr("CStruct") \{

                    has uint8 \$.response_type is rw;
                    has uint8 \$.error_code is rw;
                    has uint16 \$.sequence is rw;
            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                    method Hash \{
                        \{
                            :sequence(\$\!sequence),
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativeize(\$p6) \{
                        \$\!sequence = \$p6.sequence;
            {$p.params».p2c_init.join("\n").indent(12)}
                    }
                };
                my \$.cstruct = cstruct;

            EOCS

        my $clname = $error.attribs<name>.Str;
        $clname = $clname.lc.tc if $clname ~~ /^<upper>+$/;
        $mod.errors{$number} = $oname ~ $clname ~ "Error";
        %cstructs{$error.attribs<name>} = $clname;

        my $mcode;
        if not $oname and $error.attribs<name> eq "Request" {
            $mcode = qq:to<EOMR>;
                method message_extra \{
                    "Request 0x" ~ \$.sequence.base(16);
                }
            EOMR
        }
        else {
            $mcode = qq:to<EOMV>;
                method message_extra \{
                    "Value 0x" ~ \$.bad_value.base(16) ~
                    ", Request 0x" ~ \$.sequence.base(16);
                }
            EOMV
        }

        my @doc;
        @doc.append(MakeClassDocs($error, $clname));

        @p6classes.push(qq:to<EO6C>);
            {@doc.join("\n")}
            our class {$oname}{$clname}Error does Error[$number] is export(:DEFAULT, :errors) \{
                my \$.error_code = $number; # without the extension base number

                { @cstructs[*-1] }

                has \$.sequence;
            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

                method child_bufs \{
                    my @bufs;
            {$p.params».p2c_code.join("\n").indent(8)}
                    |@bufs;
                }

                method child_structs(Pointer \$p, \$pstruct,
                                     Real :\$left! is rw) \{
                    my @args;
                    my \$oleft = \$left;
            {$p.params».c2p_code.join("\n").indent(8)}
                    |@args;
                }

            $mcode

            }
            EO6C
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}

# Temporary, to cull TODOP6s
my %GoodReqs = :Nil(1);

sub MakeStructs($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    for $mod.xml.root.elements(:TAG<struct>) -> $struct {
        my $cstruct = "";
        my @p6fields;
        my $p6assigns = "";
        my $p6args = "";
        my $padnum = -1;
        my @reqfields;
        my params $p .= new;
        my $found_list = 0;

        for $struct.elements -> $e {
            MakeCStructField($p, $e, $padnum, $found_list);
        }
        for $struct.elements -> $e {
            if $struct.elements(:TAG<doc>) -> [ $doc ] {
                 if $doc.elements(:TAG<field>).grep(
                     {$e.attribs<name>:exists and 
                      $_.attribs<name> eq $e.attribs<name>}) -> [ $fdoc ] {
                     my $docstr =
                         ("#| $_" for $fdoc.cdata».data».Str.lines).join("\n");
                     $p{$e.attribs<name>}.p_doc = $docstr
                            if $p{$e.attribs<name>}.p_attr !~~ /^\#/;
                     $p{$e.attribs<name>}.c_doc = $docstr;
                 }
            }
        }

        @cstructs.push(qq:to<EOCS>);

                our class cstruct is repr("CStruct") \{

            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                    method Hash \{
                        \{
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativeize(\$p6) \{
            {$p.params».p2c_init.join("\n").indent(12)}
                    }
                };
                my \$.cstruct = cstruct;

            EOCS

        my $clname = $struct.attribs<name>.Str;
        $clname = $clname.lc.tc if $clname ~~ /^<upper>+$/;
        $clname = (given $clname {
            when "Str" { "String" }
            when "Notify" { "{$oname}Notify" }
            when "Modeinfo" { "{$oname}Modeinfo" }
            when "Format" { "{$oname}Format" }
            default    { $_  }
        });
        %cstructs{$struct.attribs<name>} = $clname;

        my @doc;
        @doc.append(MakeClassDocs($struct, $clname));

        my $optnew = $p.params».c2p_code.elems ?? "" !! qq:to<EO6O>;

                # Optimize leaf nodes, since there can be tens of thousands
                multi method new (Pointer \$p, Int :\$left! is rw, Bool :\$free = True) \{
                    my \$cs = nativecast(cstruct, \$p);
                    \$left -= nativesizeof(cstruct);
                    fail("Short packet.") unless \$left >= 0;
                    my \$res = self.bless(|\$cs.Hash);
                    xcb_free \$p if \$free;
                    \$res;
                }
            EO6O

        @p6classes.push(qq:to<EO6C>);
            {@doc.join("\n")}
            our class {$clname} does Struct is export(:DEFAULT, :structs) \{

                { @cstructs[*-1] }

            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

                method child_bufs \{
                    my @bufs;
            {$p.params».p2c_code.join("\n").indent(8)}
                    |@bufs;
                }

                method child_structs(Pointer \$p, \$pstruct,
                                     Real :\$left! is rw) \{
                    my @args;
                    my \$oleft = \$left;
            {$p.params».c2p_code.join("\n").indent(8)}
                    |@args;
                }

            $optnew

            }
            EO6C
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}

sub MakeReplies($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    for $mod.xml.root.elements(:TAG<request>) -> $req {
        my $cstruct = "";
        my @p6fields;
        my $p6args = "";

        my $clname = $req.attribs<name>;
        $clname = $mod.modname ~ $clname
            if $clname eq any <
                DestroyContext QueryVersion QueryExtension ListProperties
                CreateCursor GetVersion QueryBestSize SelectInput
                Enable CreateContext ChangeSaveSet PutImage CreatePixmap
            > or $mod.cname eq "present";
        my @doc;

        next unless (for $req.elements(:TAG<reply>) -> $rep {
            my $padnum = -1;
            my @reqfields;
            my params $p .= new;
            my $found_list = 0;

            state $two = 1;
            die "Two replies for request" if $two > 1;
            $two++;

            for $rep.elements -> $e {
                MakeCStructField($p, $e, $padnum, $found_list);
            }
            for $rep.elements -> $e {
                if $rep.elements(:TAG<doc>) -> [ $doc ] {
                    if $doc.elements(:TAG<field>).grep(
                        {$e.attribs<name>:exists and 
                         $_.attribs<name> eq $e.attribs<name>}) -> [ $fdoc ] {
                        my $docstr =
                             ("#| $_" for $fdoc.cdata».data».Str.lines).join("\n");
                        $p{$e.attribs<name>}.p_doc = $docstr
                            if $p{$e.attribs<name>}.p_attr !~~ /^\#/;
                        $p{$e.attribs<name>}.c_doc = $docstr;
                    }
                }
            }

            @doc.append(MakeClassDocs($rep, $clname ~ "Reply"));

            $p<pad0_0>.c_attr = 'has uint8 $.pad0_0;' unless $p.params[0]:exists;
            $p.params.splice(1,0,
                param.new(:name<sequence>,:c_attr('has uint16 $.sequence is rw;')),
                param.new(:name<length>,:c_attr(
                    'has uint32 $.length is rw;' ~ "\n" ~
                    'constant length___maxof = 0xffffffff;' ~ "\n"))
            );
            $p.params.unshift(
                 param.new(:name<response_type>,
                           :c_attr('has uint8 $.response_type is rw;')));

            @cstructs.push(qq:to<EOCS>);

                    our class cstruct is repr("CStruct") \{

                {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                        method Hash \{
                            \{
                {$p.params».c2p_arg.join(",\n").indent(16)}
                            }
                        }
                        method nativeize(\$p6) \{
                {$p.params».p2c_init.join("\n").indent(12)}
                        }
                    };
                    my \$.cstruct = cstruct;
                EOCS


            @p6classes.push(qq:to<EO6C>);
                {@doc.join("\n")}
                our class {$clname}Reply
                    does Reply[{$oname}OpcodeEnum::{$oname}Opcode({$req.attribs<opcode>})]
                    is export(:DEFAULT, :replies) \{

                { @cstructs[*-1] }

                    has \$.sequence is rw;
                {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

                    method child_bufs \{
                        my @bufs;
                {$p.params».p2c_code.join("\n").indent(8)}
                        |@bufs;
                    }

                    method child_structs(Pointer \$p, \$pstruct,
                                         Real :\$left! is rw) \{
                        my @args;
                        my \$oleft = \$left;
                {$p.params».c2p_code.join("\n").indent(8)}
                        |@args;
                    }

                }
                EO6C

%GoodReqs{"$oname $clname"}++ unless @p6classes[*-1] ~~ /TODOP6/;

        });
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}

sub MakeRequests($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    for $mod.xml.root.elements(:TAG<request>) -> $req {
        my $cstruct = "";
        my @p6fields;
        my $p6assigns = "";
        my $p6args = "";
        my $padnum = -1;
        my @reqfields;
        my params $p .= new;
        my $found_list = 0;

        # We use the core API for this
        next if $req.attribs<name> eq "SetupAuthenticate";

        for $req.elements -> $e {
            MakeCStructField($p, $e, $padnum, $found_list);
        }
        for $req.elements -> $e {
            if $req.elements(:TAG<doc>) -> [ $doc ] {
                 if $doc.elements(:TAG<field>).grep(
                     {$e.attribs<name>:exists and 
                      $_.attribs<name> eq $e.attribs<name>}) -> [ $fdoc ] {
                     my $docstr =
                         ("#| $_" for $fdoc.cdata».data».Str.lines).join("\n");
                     $p{$e.attribs<name>}.p_doc = $docstr
                            if $p{$e.attribs<name>}.p_attr !~~ /^\#/;
                     $p{$e.attribs<name>}.c_doc = $docstr;
                 }
            }
        }

        $p<pad0_0>.c_attr = 'has uint8 $.pad0_0;' unless $p.params[0]:exists;
        $p.params.splice(1,0,
             param.new(:name<length>,:c_attr(
                       'has uint16 $.length is rw;' ~ "\n" ~
                       'constant length___maxof = 0xffff;' ~ "\n")));
        $p.params.unshift(
             param.new(:name<major_opcode>
                       :p2c_init("\$!major_opcode = {$req.attribs<opcode>};")
                       :c_attr('has uint8 $.major_opcode is rw;'))
        );

        @cstructs.push(qq:to<EOCS>);

                our class cstruct is repr("CStruct") \{

            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                    method Hash \{
                        \{
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativeize(\$p6) \{
            {$p.params».p2c_init.join("\n").indent(12)}
                    }
                };
                my \$.cstruct = cstruct;
            EOCS

        my $clname = $req.attribs<name>;
        $clname = $mod.modname ~ $clname
            if $clname eq any <
                DestroyContext QueryVersion QueryExtension ListProperties
                CreateCursor GetVersion QueryBestSize SelectInput
                Enable CreateContext ChangeSaveSet PutImage CreatePixmap
            > or $mod.cname eq "present";
        my $isvoid = not $req.elements(:TAG<reply>);

        my @doc;
        @doc.append(MakeClassDocs($req, $clname ~ "Request"));

        @p6classes.push(qq:to<EO6C>);
{%GoodReqs{$isvoid ?? "Nil" !! "$oname $clname"} ?? "" !! "# TODOP6 Reply has TODOP6s"}
            {@doc.join("\n")}
            our class {$clname}Request
                does Request[{$oname}OpcodeEnum::{$oname}Opcode({$req.attribs<opcode>}),
                             {$mod.cname eq "xproto"
                                  ?? "xcb_extension_t"
                                  !! '$xcb_' ~ $mod.cname ~ "_id"},
                             {$isvoid.gist}]
                is export(:DEFAULT, :replies) \{

                my \$.reply{ $isvoid ?? ";" !! " = " ~ $clname ~ "Reply" ~ ";" }

                { @cstructs[*-1] }

                has \$.sequence is rw;
            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

                method child_bufs \{
                    my @bufs;
            {$p.params».p2c_code.join("\n").indent(8)}
                    |@bufs;
                }

            }
            EO6C
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}


sub Output ($mod) {
    my $out = "./lib/X11/XCB/$mod.outname()".IO;
    $out .= open(:w);
    $out.print($mod.prologue ~ "\n");

    $out.print( qq:to<EOOC> ) if $mod.opcodes;
        our class { $mod.extension ?? $mod.modname !! "" }OpcodeEnum is export(:opcodes) \{
            our enum { $mod.extension ?? $mod.modname !! "" }Opcode is export(:enums) «
        { (":{.value}({.key})\n" for $mod.opcodes.sort(+*.key)).join.indent(8) }
            »;
        }

        EOOC
    $out.print($mod.enums.join);
    $out.print($mod.typedefs.join);
    $out.print($mod.p6classes.grep({$_ !~~ /TODOP6/}).join);
    $out.print(qq:to<EOEH>);
        our \$errorcodes = :\{
        { (for $mod.errors.sort(+*.key) {
              $_.key ~ " => " ~ $_.value
           }).join(",\n").indent(4) }
        };
        EOEH
    say($mod.p6classes.grep({$_ ~~ /TODOP6/}).join);
    $out.close;
}