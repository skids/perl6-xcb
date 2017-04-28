#!/usr/bin/env perl6
use lib 'lib';
use XML;
use X11::XCBquirks;

# Places we are likely to find XCB-XML files
my @xmlpaths = "/usr/share/xcb";

my $generator_version = v0.alpha; # NOTE: I only update this on git tags
my $append_version; # Gets appended to extension version

my %embeddable; # List of structs which are embeddable with HAS

class param is rw {
    has $.name;
    has $.c_attr = |();
    has $.c_type;
    has $.c_offset = 0; # not a real offset, just correct alignmentwise
    has $.p_attr = |();
    has $.p_subclasses = |();
    has $.c_doc = |();
    has $.p_doc = |();
    has $.c2p_arg = |();
    has $.c2p_code = |();
    has $.p2c_init = |();
    has $.p2c_code = |();
    has $.fd_init = |();
    has $.fd_send = |();
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
    has $.init_offset = 0;  # Length of top fields fixed up later
    has params $.parent;    # For nesting
    has $.child_structs is rw = [];  # extra code at top of child_structs method
    method find_param_owner ($name) {
        my $p = self;
        while $p.defined {
            last if $p{$name}:exists;
            $p = $p.parent;
        }
        $p;
    }
}

# What trailing alignment would C use at this point?
# Note we do not know if a larger field will come along,
# but so far, it seems the XCBXML always is explicit in
# places where this answer does not equal the intrastructure
# alignment, though that may not be intentional.
my sub c_alignment_rules($p) {
    ($p.init_offset, |(.c_offset for $p.params)).rotor(2 => -1).map(
        -> ($a, $b) {$b - $a}
    ).max;
}

# How to translate an occluded enum and its class to perl6 code,
class ocmunge {
    has $.cname;                  # name of the class
    has $.cstruct;                # which role $.cstruct to use
    # extra does when we are part of a ocrole (see below)
    has $.sharedrole = $!cstruct eq $!cname ?? "" !! $!cstruct;
    has $.prole_name;             # paramtric role for occlude
    has $.prole_val;              # value to paramterize it with
}

# A role shared by multiple union members.
class ocrole {
    has $.name;
    has ocmunge @.users is rw;
    has $.done is rw = False;
}

# For unions where variants are selected by an enum, we occlude
# that enum behind a role/pun which causes variant type
# objects to numify into their corresponding enum value.
# This reduces namespace fudgery and is simpler to use,
# as we have less identifiers jostling around.
class occlude {
    has $.class;   # Class representing the union

    # Class of generic variant that pads out a union,
    # or if none exists, == $.class
    has $.generic = $!class;

    # Class where we find this union as a member,
    # or if this is a self-multiplexing union, == $.generic
    has $.envelope = $!generic;

    has $.typer;   # Field in $.envelope used to decide which subtype to use
    has $.typee;   # Field in $.envelope with union body or remainder therof
    has $.enum;    # Name of enum used in $.typer to choose types
    has &.toenum = { $_ };   # converts subtype names to enum values
    has &.totype = { $_ };   # converts enum values to subtype names
    has &.torole = &!totype; # converts enum values to shared union subtypes
    has @.cattrs is rw;
    has $.needs_cstruct = "";
    has $.cstruct_done = False;
    has $.typermask = 0xff;  # Workaround for broken unsigned natives

    method cstruct {
        return "" if $!cstruct_done;
        $!cstruct_done = True;

        $!needs_cstruct
        ~
        "class {$.class}::cstruct does cpacking is repr('CUnion') \{\n"
        ~
        ("HAS $_\:\:cstruct \$\.$_;" for @!cattrs).join("\n").indent(4)
        ~
        q:to<EOWS>

            method wiresize {
                state $ = max(|(wiresize($_.type) for ::?CLASS.^attributes))
            }
        EOWS
        ~
        "\n}\n";
    }

    # Generate map of subtype names to does clauses to add to them.
    # While we are at it, harvest the subtype names for the cstruct.
    method doeses($enum) {
        my %res;
        for $enum.elements(:TAG<item>) -> $item {
            my $v = $item.elements[0];
            my $r = &!torole($item.attribs<name>);
            my $n = &!totype($item.attribs<name>);
            @!cattrs.push($n);
            if $v.name eq "bit" {
                $v = 1 +< $v.nodes[0].text;
            }
            else {
                $v = $v.nodes[0].text;
            }
            my $obj = ocmunge.new(
                :cname($n), :cstruct($r),
                :prole_name($.class), :prole_val($v)
            );
            if ($r ne $n and %res{$r}:exists) {
                %res{$r}.users.push($obj);
            }
            elsif ($r ne $n) {
                %res{$r} = ocrole.new(:name($r), :users($obj));
            }
            else {
                %res{$n} = $obj;
            }
        }
	%res
    }

    # Generate the glue code implementing the guts of the role
    # and type coercers.
    method gluecode {
        my $rname = $.class;
        my $morph = "";
        my $does = "";

        %embeddable{$.generic} = True; # A bit hacky to do this here
        %embeddable{$.class} = True;

        if $.envelope eq $.generic {
            $morph = self.morphstructor;
        }
        else {
            $does = "does MonoStruct ";
        }
        qq:to<EOOC>;

            # Implement a collection of classes whose type objects
            # act like the values of the enum $.enum, rather than
            # providing that enum.
            my \%$rname;

            role $rname\[Int \$i = -1] $does\{
                multi method Numeric (::?CLASS:U:) \{ \$i }
                multi method Int (::?CLASS:U:) \{ \$i }

                \%$rname := :\{} unless \%$rname.defined;
                \%$rname\{\$i} = ::?CLASS unless \$i < 0;

            {$morph.indent(4)}
            }

            multi sub $rname (Numeric() \$i) is export \{
                \%$rname\{\$i.Int};
            }
            multi sub $rname ($rname\:D \$r) is export \{
                \%$rname\{\$r.WHAT.Numeric};
            }
            multi sub $rname () is export \{
                \%$rname.sort.list;
            }

            EOOC

    }

    method fixparams (params $p is rw, :$subclass?) {
        if $.envelope =:= $.generic {
            if $subclass.defined {
                $p{$.typer}.p_attr = |();
                $p{$.typer}.c2p_arg = |();
                $p{$.typer}.p2c_init = "\$\!$.typer = +\$p6.WHAT;";
                return;
            }
        }
        return if $.envelope =:= $.generic or $subclass.defined;

        $p{$.typer}.p2c_init = "\$\!$.typer = +\$p6.$.typee.WHAT;";
        $p{$.typer}.p_attr = |();
        $p{$.typer}.c2p_arg = |();
        $p{$.typee}.c2p_arg = qq:to<EOPA>;
            :{$.typee}(\%$.generic\{\$\!$.typer}.new(nativecast(Pointer[uint8],\$\!$.typee),
                :left({$.generic}::cstruct.wiresize), :!free))
            EOPA


    }

    method morphstructor () {
        my $primerstruct = $.generic =:= $.class ??
           '::?CLASS.WHO<cstruct>' !! "$.generic\.cstruct";
        qq:to<EOCC>;
            multi method new (Pointer \$p, Int :\$left! is rw, Bool :\$free = True) \{
                my \$to = $primerstruct;
                # We assume typer is not in a dynamic part of the struct
                my \$cs = nativecast(\$to, \$p);
                fail("Short packet.") unless \$left >= \$to.wiresize;
                my \$variant = \%$.class\{\$cs.$.typer +& $.typermask};
		\$variant.new(\$p, :\$left, :\$free);
            }

            EOCC
    }
}

# This part has to be done by hand... information for each
# self-multiplexed union in each module
my %Occludes is default(|());
%Occludes.append:
    "randr" => ( occlude.new(:generic<NotifyData>, :class<NotifyData>,
                             :envelope<NotifyEvent>,
                             :typer<subCode>, :typee<u>, :enum<Notify>)
               ),
    "xkb"   => ( occlude.new(:generic<SIAction>, :class<Action>,
                             :typer<type>, :typee<data>, :enum<SAType>,
                             :toenum({ $_ ~~ /^SA(.*)$/; my $it = $/[0].Str; $it ~~ s/IsoLock/ISOLock/; $it }),
                             :totype({ $_ ~~ s/ISOLock/IsoLock/; "SA$_" }))
               ),
    "xkb"   => ( occlude.new(:generic<CommonBehavior>, :class<Behavior>,
                             :typer<type>, :typee<data>, :enum<BehaviorType>,
                             :toenum({ $_ ~~ /^(.*?)Behavior$/; $/[0].Str}),
                             :torole({ $_ ~~ /(.*?)\d*$/; $/[0] ~ "Behavior" }),
                             :totype({ $_ ~ "Behavior" }))
               )
;

class mod {
    has $.xml;
    has $.cname;
    has $.outname;
    has $.modname;
    has $.extension;
    has $.xextension;
    has $.prologue is rw;
    has @.enums is rw;
    has %.renums is rw;
    has @.typedefs is rw;
    has %.subclasses is rw;
    has @.cstructs is rw;
    has @.p6classes is rw;
    has $.epilogue is rw;
    has %.opcodes is rw;
    has %.errors is rw;
    has %.events is rw;
    has %.expexp is rw;   # explicit EXPORT packages
    has %.xge is rw;
    has %.occlude is rw;  # resulting munges for occlusions
    has @.occludes is rw; # tweaks for occluding certain enums
    has Array %.rolecstruct is rw;

    my @.allmods; # Global state: instances of this class

    method new (|c) {
        my $res = self.bless(:occludes(|%Occludes{c<cname>}), |c);
        @.allmods.push($res);
        $res;
    }

    # Look up a mod by its name (for cross-module references e.g. "shape:KIND")
    method of_str ($str) {
        @.allmods.first(*.cname eq $str);
    }

    # Rarely, we need to know what mod we are working on.
    # Passing that through as state is bloaty.
    method of_xml ($xml) {
        my $cname = $xml.ownerDocument.root.attribs<header>;
        self.of_str($cname);
    }

    multi method renum(Str $enum) {
        if $enum ~~ m/(<-[:]>+)\:(.*)/ {
            self.of_str($/[0]).renums{$/[1]}.key;
        }
        else {
            %.renums{$enum}.key;
        }
    }

    multi method renum(XML::Node $xml, Str $enum) {
        self.of_xml($xml).renum($enum);
    }

    multi method renumv(Str $enum, Str $enumv) {
        if $enum ~~ m/(<-[:]>+)\:(.*)/ {
            self.of_str($/[0]).renums{$/[1]}.value.{$enumv};
        }
        else {
            %.renums{$enum}.value.{$enumv};
        }
    }

    multi method renumv(XML::Node $xml, Str $enum, Str $enumv) {
        self.of_xml($xml).renumv($enum, $enumv);
    }
}

class bitswitch {
    has $.xml;
    has $.casename;
    has $.prologue is rw;
    has @.cstructs is rw;
    has @.p6classes is rw;
    has $.epilogue is rw;
}

# Keep track of XML coverage during devel.
my $TODOP6 = "TODOP6_0";

sub guess_xcbxml_version (IO::Path $dir, @xmlfiles) {
    my $res;
    my $news = $dir.child("NEWS");
    if $news.e {
        note "Taking release version from $news";
        my $n = $news.IO.lines.first(/Release\s+\d/);
        $res = Version.new($/[0])
            if ($n ~~ /Release\s+(\d+\.\d+)\s/)
    }
    if $dir.child(".gitattributes").e or $dir.child(".git").e {
        note "Your XCB XML is in a git repo.  Refining version using git.";
        my $headsha =
            (run "git", "rev-parse", "HEAD", :cwd($dir), :out).out.slurp-rest;
        my $n;
        without $res {
            my $log = (run "git", "log", "-n", "1000", :cwd(~$dir), :out);
            $n = $log.out.lines.first: /Release\s+xcb\-proto\s+\d/;
            $log.sink;
            $res = Version.new($/[0])
                if ($n ~~ /Release\s+xcb\-proto\s+(\d+\.\d+)/);
        }
        with $res {
            my $tagsha = (
                    run "git", "rev-list", "-n", 1, $res, :cwd($dir), :out
                ).out.slurp-rest;
	    if $headsha ne $tagsha {
                # We are ahead of release, treat as a pre-release for next one
                $res = Version.new(($res.parts[0..*-2], $res.parts[*-1] + 1,
                                    "prior").join("."));
            }
            else {
                $res = Version.new(($res.parts[0..^*], 0).join("."));
            }
        }
    }
    without $res {
        # No NEWS or git.  Assume it is a release.  Use a heuristic.
        note "Warning: Wild guess at xcbxml version.  Better to provide on CLI.";
        my $xproto = @xmlfiles[0].slurp;
        $res = v1.10.0 if $xproto ~~ /GeGeneric/;
        $res = v1.12.0 if $res.defined and $xproto ~~ /my_example\(xcb_connection_t\s\*c\,/;
    }
    with $res {
        $res ~= ".0" unless +$res.parts > 2
    }
    die "Please provide --version=x.y.alpha on CLI" without $res;
    $res;
}

sub MAIN (:$xmldir? is copy, :$version? is copy) {
    my $distdir;
    unless $xmldir {
        $xmldir = @xmlpaths.grep(*.IO.d);
    }
    if $xmldir {
        $xmldir .= IO;
        $distdir = $xmldir;
        if $xmldir.child("NEWS").f
            and not +$xmldir.dir.grep(*.extension eq any <XML xml>) {
            $xmldir .= child("src");
        }
    }
    else {
        die q:to<EOHELP>;
            Could not find directory with XCB XML files.
            Please specify it as an option e.g. :xmldir(/path/to/xml)
            EOHELP
    }

    my @xmlfiles = $xmldir.dir.grep(*.extension eq any <XML xml>).grep(*.basename ne any <xproto.xml>);
    @xmlfiles.unshift(|$xmldir.dir.grep(*.basename eq any <xproto.xml>));

    if ($version) {
        $version ~= ".0" unless $version ~~ /.+..+..+/;
        $version = Version.new($version);
    }
    else {
        $version = guess_xcbxml_version($distdir,@xmlfiles);
    }
    $append_version = (|$version.parts,|$generator_version.parts).join(".");
    note "Module versions will be appended with $append_version";
# for fast testing
#@xmlfiles = @xmlfiles.grep(*.basename eq any <xproto.xml>);
    my @mods;
    @mods.push(MakeMod($_)) for @xmlfiles;
    MakeImports($_, @mods) for @mods;
    GetOpcodes($_) for @mods;
    MakeEnums($_) for @mods;
    MakeTypeDefs($_) for @mods;
    MakeStructs($_) for @mods;
    MakeErrors($_) for @mods;
    MakeErrors2($_) for @mods;
    MakeEvents($_) for @mods;
    MakeEvents2($_) for @mods;
    MakeReplies($_) for @mods;
    MakeRequests($_) for @mods;
    MakeEpilogue($_) for @mods;
    Output($_) for @mods;
}

sub makemeth_child_bufs(params $p, :$indent = 0) {

    my $p2c_code = $p.params».p2c_code;
    with $p2c_code.first(*.chars) { qq:to<EOCB>.indent($indent) }
        method child_bufs \{
            my @bufs;
        {$p2c_code.join("\n").indent($indent + 4)}
            |@bufs;
        }
        EOCB
    else { "" }
}

sub makemeth_child_structs(params $p, :$indent = 0) {

    my $c2p_code = $p.params».c2p_code;
    with $c2p_code.first(*.chars) { qq:to<EOCS>.indent($indent) }
        method child_structs(Pointer \$p, \$pstruct, Real :\$left! is rw) \{
            my @args;
            my \$oleft = \$left;
        {$p.child_structs.join("\n").indent($indent + 4)}
        {$c2p_code.join("\n").indent($indent + 4)}
            |@args;
        }
        EOCS
    else { "" }
}

# Special quirks and workarounds
my %quirks = "ClientMessageData" => &makeClientMessageData, "list:preserve_entries" => &makeParasiticList, "list:preserve" => &makeParasiticList;

sub MakeMod ($xml) {
    my $outname;
    my $cname;
    my $modname;
    my $extension;
    my $xextension;
    my $xmltree = from-xml-file($xml.Str);
    die "root tag of xcb not found in file {$xml.Str}"
        unless $xmltree.root.name eq "xcb";

    my $ver;
    given $xmltree.root {
        $cname = $modname = .attribs<header>;
        if $modname eq "xproto" {
            $modname = "XProto";
            $extension = False;
            $ver = "11";
        }
        else {
            $extension = .attribs<extension-name>;
            $xextension = .attribs<extension-xname>;
            $modname = $extension;
	    $ver = .attribs<major-version> ~ "." ~ .attribs<minor-version>;
        }
        $outname = "$modname.pm6";
    }
    $ver = "\:ver<$ver\.$append_version>";

    # Putting a version on XProto seems to break things.
    $ver = "" if $modname eq "XProto";

    my $prologue = qq:to<EOP>;
        module X11::XCB::$modname$ver \{

        use NativeCall;
        use X11::XCB :internal, :DEFAULT;
        EOP

    if $extension {
        $prologue ~= qq:to<EOE>;

            # Should work, does not on multiple levels, and getting
            # rid of it lets us not be arsed with the corresponding .so
            # our constant \$xcb_{$cname}_id is export(:internal) :=
            #    cglobal("xcb-{$cname}", "xcb_{$cname}_id", xcb_extension_t);

            # We need a "constant" container that can be passed at
            # compile time through role parameters, but then initialized at
            # runtime.  All attempts to send a container and initialize
            # it later have failed.  So we use a sub.
            my \$xcb_{$cname}_id_cache;

            # Keep a nativecasted alias resident because this may(?) keep
            # storage from moving around inside the GC.
            my \$xcb_{$cname}_id_anchor;
            my \$xcb_{$cname}_id_name_anchor;

            our sub xcb_{$cname}_id \{
                with \$xcb_{$cname}_id_cache \{
                    \$xcb_{$cname}_id_cache;
                }
                else \{
                    # For some awful reason (?) we need to cram
                    # the extension name in with a crowbar.
                    \$xcb_{$cname}_id_name_anchor = "$xextension\\x00".encode("utf8");
                    \$xcb_{$cname}_id_cache =
                        xcb_extension_t.new(
                            :name(nativecast(Pointer,
                                             \$xcb_{$cname}_id_name_anchor
                                            ).Int), :num(0));
                    \$xcb_{$cname}_id_anchor =
                        nativecast(CArray[uint8], \$xcb_{$cname}_id_cache);
                    \$xcb_{$cname}_id_cache;
                }
            }

            our sub extension_t \{ xcb_{$cname}_id() }

            EOE
    } else {
        my $cheez;
	for %X11::XCBquirks::EnumValueConst -> (:$key, :$value) {
	    $cheez ~= "constant $key is export(:internal, :enums) = $value;\n"
	}
        $prologue ~= qq:to<EOE>;

            # Provide a way to skirt around lots of enums using these
	    $cheez
            EOE
    }

    mod.new(:xml($xmltree), :$outname, :$modname, :$extension,
            :$xextension, :$cname, :$prologue);
}

our %nctypemap = "shape:KIND" => "uint8";
sub NCtype ($t is copy) {
    my $o = $t;
    $t = %nctypemap{$t} while %nctypemap{$t}:exists;
    $t ~~ s/^CARD/uint/;
    $t ~~ s/^BOOL$/uint8/;
    $t ~~ s/^BYTE$/uint8/;
    $t ~~ s/^INT/int/;
    $t ~~ s/^char$/uint8/;
    $t ~~ s/^FLOAT/num/;
    $t ~~ s/^float$/num32/;
    $t ~~ s/^double$/num64/;
    %nctypemap{$o} = $t unless $o eq $t;
    ($t eq any <int8 int16 int32 int64
               uint8 uint16 uint32 uint64
               long longlong num32 num64
               Str CArray[int32] Pointer[void]
               bool size_t>) ?? $t !! "TODOP6";
}

# Sometimes we want to have the type object here in the generator
# rather than a string.
our %nctypemap_eval;
sub NCevaltype ($t) {
    use MONKEY-SEE-NO-EVAL;

    unless %nctypemap_eval{$t}:exists {
        my $typestr = NCtype($t);
        %nctypemap_eval{$t} = EVAL $typestr;
    }
    %nctypemap_eval{$t}
}

# Also, easy access to nativesizeofs
our %nctypemap_size;
sub NCsizetype ($t) {
    use NativeCall;
    unless %nctypemap_size{$t}:exists {
        my $typesize = nativesizeof(NCevaltype($t));
        %nctypemap_size{$t} = $typesize;
    }
    %nctypemap_size{$t}
}
sub NCmasktype ($t) {
    (1 +< (NCsizetype($t) * 8)) - 1
}

sub MakeTypeDefs ($mod) {
    for (|$mod.xml.root.elements(:TAG<xidtype>),
         |$mod.xml.root.elements(:TAG<xidunion>)) -> $e {
        my $t = $e.attribs<name>;
        %nctypemap{$t} = $t ~ "ID";
        %nctypemap{$t ~ "ID"} = "uint32";
        $t = %nctypemap{"glx:$t"} = "Glx$t" if $mod.cname eq "glx";
        $t = %nctypemap{"record:$t"} = "Record$t" if $mod.cname eq "record";
        %nctypemap{"xproto:$t"} = $t if $mod.cname eq "xproto";
        $t = %nctypemap{$t} = $t ~ "ID";
        %nctypemap{$t} = "uint32";
        $mod.typedefs.push: "constant $t" ~
            " is export(:internal, :ctypes) = uint32;\n";
    }
    for $mod.xml.root.elements(:TAG<typedef>) -> $e {
        %nctypemap{$e.attribs<newname>} = $e.attribs<oldname>;
        my $t = NCtype($e.attribs<oldname>);
        # XXX figure this out without a manual list
        if ($e.attribs<newname> ~~ /Behavior|^SA/) {
            $mod.subclasses{$e.attribs<newname>} = $e.attribs<oldname>;
        }
        else {
            $mod.typedefs.push: "constant {$e.attribs<newname>}" ~
                " is export(:internal, :ctypes) = $t;\n";
        }
    }
}



sub MakeEnums ($mod) {

    # Fixup/perlify for enum type names that conflict
    my sub fix_name($enum) {
        return $mod.modname ~ $enum
            if $mod.cname eq "present" and $enum eq any <Event EventMask>
            or $mod.cname eq "randr"   and $enum eq any <Connection>
            or $mod.cname eq "glx"     and $enum eq any <GC>
            or $enum eq any <Control Cursor>;

        return "GCField" if $mod.cname eq "xproto" and $enum eq any <GC>;

        return "ScreenSaverState"
            if $enum eq "ScreenSaver";

        return $enum;
    }

    # Keep some things in their own namespace rather than munging them
    my sub fix_export($mod, $enum) {
        my @elements = $enum.elements(:TAG<item>);
        my @res = $(:DEFAULT,), " is export(:enums)";
	my $ename = $mod.modname ~ "::" ~ $enum.attribs<name>;

        # Perl6 things that can be overidden, but avoid surprises
        # TODOP6: will have to go look for more of these pre-publication
        # TODOP6: better exemption logic here rather than cherry picking
        if $ename ne "XProto::CW" {
            if @elements.first(*.attribs<name> eq any <
                 Cursor
                >) {
                @res[1] = " is export(:danger)";
            }
        }

        # conflicts within or between modules
	with %X11::XCBquirks::EnumExports{$ename} {
            @res[0] = $_;
        }
	with %X11::XCBquirks::EnumValueExports{$ename} {
# To workaround RT#127305 we cannot do this inline
#	    @res[1] = ' is export'
#                ~ @$_.perl
# Instead add it to the post-scope explicit EXPORTs
            @res[1] = '';

            my $evc = "our constant {$enum.attribs<name>} = ::X11::XCB::{$ename}Enum";
            for @(@res[0]) -> $eve {
                $mod.expexp{"EXPORT::" ~ $eve.key} ~= "$evc;\n";
            }
	    @res[0] = '';
            for @elements -> $eve {
                my $evf = fix_valname($eve.attribs<name>, $enum.attribs<name>);
                my $n = $ename ~ "Enum::" ~ $enum.attribs<name>;
                $evc = "our constant $evf = ::X11::XCB::"
                    ~ $n ~ "::" ~ $evf ~ ";\n";
                for @(%X11::XCBquirks::EnumValueExports{$ename}) {
                    $mod.expexp{"EXPORT::" ~ $_.key} ~= $evc;
                    $mod.expexp{$n ~ "::EXPORT::" ~ $_.key} ~= $evc;
                }
            }
        }

        if @res[0] {
	    @res[0] = " is export{@(@res[0]).perl}"
        }

        |@res;
    }

    # Fixup/perlify for enum value names that conflict intra-module
    my sub fix_valname($item, $from) {
        return "$from$item" if $item ~~ /^\d+/;

        # A bunch of masks named the same as their values
        return $item ~ "Mask" if $from
            eq any <PresentEventMask SelectionEventMask Explicit
                    CursorNotifyMask NotifyMask SetOfGroup
                    xkbControl NameDetail SAIsoLockNoAffect>
            or $mod.cname eq "xkb" and $from eq any <EventType>;
        return $item ~ "High" if $from eq 'BoolCtrlsHigh';
        return $item ~ "Low" if $from eq 'BoolCtrlsLow';
        return substr($from,0,*-5) ~ $item if $from
            eq any <IMModsWhich IMGroupsWhich>;
        return substr($from,0,3) ~ $item if $from
            eq any <NKNDetail GBNDetail>;
        return $from ~ $item if $from
            eq any <SAType>;
        return $item ~ "Notify" if $from eq "Notify" and
            $mod.cname eq "randr";

        return "CW$item"      if $from eq any <CW ConfigWindow>;
        return "GC$item"      if $from eq any <GCField>;

        return "$from$item"
            if $mod.cname eq "xkb" and $from eq any <Groups>
            or $from eq any <
                GrabMode LineStyle FillStyle CapStyle JoinStyle GC SA SAIsoLockFlag CP
            >
            # Individual values
            or $item eq "PointerRoot" and $from eq "InputFocus"
            or $item eq any <
                Off On Any Lock Shift Insert Delete Control Pointer Cursor
            >
	    or %X11::XCBquirks::EnumValueConst{$item}:exists;

        return "VISUAL"       if $item eq "VISUALID"; # restore consistency

        return $item;
    }

    for $mod.xml.root.elements(:TAG<enum>) -> $e {
        my $ename = $e.attribs<name>;

	# Eliminate silly single-value enums
	if $e.elements.elems == 1 {
            with %X11::XCBquirks::EnumValueConst{$e.elements[0].attribs<name>}
            {
	        next if $_ == +$e.elements[0].nodes[0].text;
            }
        }

        if $mod.occludes.first(*.enum eq $ename) -> $occlude {
            $mod.occlude.append($occlude.doeses($e));
            $mod.enums.push($occlude.gluecode);
        }
        else {
            $ename = fix_name($ename);
	    $mod.renums{$e.attribs<name>.Str} = ($ename => { });
            (my $export_ext, my $export_int) = fix_export($mod,$e);

            $mod.enums.push:
            "our package {$ename}Enum$export_ext \{\n" ~
            "    our enum {$ename}$export_int «\n        " ~
            (for $e.elements(:TAG<item>) -> $item {
                state $lastval = -Inf;
                my $v = $item.elements[0];
                my $n = $mod.renums{$e.attribs<name>.Str}.value{$item.attribs<name>}
                      = fix_valname($item.attribs<name>, $ename);

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
}

sub MakeImports($for, @mods) {
    my @imports = $for.xml.root.elements(:TAG<import>);
    my $res = "";
    for @imports -> $cname {
        my $from = @mods.first(*.cname eq $cname.contents);
        $res ~= "use X11::XCB::$from.modname() :internal;\n";
    }
    $for.prologue ~= $res;

    for $for.occludes.grep({$_.generic !=:= $_.class}) {
        $for.prologue ~= "class {$_.generic} \{...}\n"
    }
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
   return "TODOP6 Not one root op" if $f.elements».name.join(" ") ne any <op unop>;
   build_op($f.elements[0]);
}

my %cstructs = "sync:INT64" => "Counter64", "Action" => "Action", "Behavior" => "Behavior", "NotifyData" => "NotifyData";

sub c2p_parse_dynamic(params $p, $type) {
    if %cstructs{$type}:exists {
        "$type\.new(Pointer.new(\$p + \$oleft - \$left), :\$left, :!free);"
    }
    else {
        my $nctype = NCtype($type);
        qq:to<EOPD>;
            (if \$left >= wiresize($nctype) \{
                 LEAVE \{ \$left -= wiresize($nctype); }
                 nativecast(Pointer[$nctype], Pointer.new(\$p + \$oleft - \$left)).deref
             }
             else \{ die "Short Packet" }
            );
            EOPD
    }
}

sub MakeSparseArray(params $p, $name, $type, $kfr) {
    my $keyname;
    my $keyiter;
    my $keypush;
    my $keyfrob;
    my $keyparm;
    my $keyvar;

    if ($kfr.name eq "fieldref") {
        $keyname = $kfr.contents.Str;
        my $owner = $p.find_param_owner($keyname);
        $keyparm = $owner{$keyname};

        if $p !=:= $keyparm {
            # Direct use of field from parent structure.  Go back
            # into the parent code and make a dynamic variable to
            # carry it into our scope.
            $keyvar = "\$\*parent___$keyname";
            $owner.child_structs.push:
                "my $keyvar = \$pstruct\.$keyname;";

        }
        else {
            $keyvar = "\$\!$keyname";
        }

        $keyiter = "$keyvar\.polymod(2 xx *)";
        $keyfrob = "$keyvar +|= +?\$_ +< \$++;";
        $p{$name}.p_attr =
            "has \@\.$name\[{(NCsizetype($keyparm.c_type) * 8)}];";
        $p{$keyname}.p2c_init = "$keyvar = 0;\n";
    }
    else {!!!}
    $p{$name}.c_attr = "# Dynamic array $name sparsed by bits in $keyname";
    $p{$name}.p2c_code = "\@bufs.append: .bufs if .defined for \@\!$name;";
    $p{$name}.c2p_code = qq:to<EOKI>;
        @args.append: \:$name\[(for $keyiter \{
            if (\$_) \{
        {c2p_parse_dynamic(params.new(:parent($p)), $type).indent(8)}
            }
            else \{
                Nil
            }
        })];
        EOKI
    $p{$keyname}.c2p_arg = "# $keyname bitmaps definedness of \@\.$name.keys";
    $p{$keyname}.p2c_init ~= qq:to<EOKP>;
        for \$p6\.$name\.map(*.defined) \{
            $keyfrob
        };
        EOKP
}

sub MakeCStructField(params $p, $f, $padnum is rw, $dynsize is rw, $rw = " is rw") {
    given ($f.isa(XML::Comment) ?? "pad" !! $f.name) {
        my $name = $f.can("attribs") ?? $f.attribs<name> !! $_;
        my $type = $f.can("attribs") ?? $f.attribs<type> !! $_;
        when "field"|"exprfield" {
            use NativeCall;

            # Deal with special mutant cases
            if %quirks{$type}:exists {
                %quirks{$type}($p, $f, :$padnum);
                succeed
            }
            my $offset =
                +$p.params ?? $p.params[*-1].c_offset !! $p.init_offset;
            my $align = c_alignment_rules($p);

	    $p{$name}.c_type = NCtype($type);
            $p{$name}.c_attr = "has {NCtype($type)} \$.$name$rw;";
            $p{$name}.p_attr = "has \$.$name is rw;";

            if %cstructs{$type}:exists and not %embeddable{$type} {
		$dynsize = True;
            }

            if $dynsize {
                $p{$name}.c_attr =
                    "# $type \$.$name outside cstruct";
                $p{$name}.p2c_code = qq:to<EOPC>;
                    \{
                        my \$cl = class :: is repr("CStruct") \{
                           has {NCtype($type)} \$.foo;
                        }
                        my \$ca = nativecast(CArray[uint8],
                                            \$cl.new(:foo(\$.$name)));
                        @bufs.push:
                            Blob.new(\$ca[^wiresize({NCtype($type)})])
                    }
                    EOPC
            }
            else {
                $p{$name}.c2p_arg = ":{$name}(\$\!$name)";
                $p{$name}.p2c_init = "\$\!{$name} = \$p6.{$name};";
            }

            if %cstructs{$type}:exists {
                $offset += $align;
                my $pptype = %cstructs{$type};
                if $dynsize {
                    $p{$name}.c2p_code = qq:to<EOPC>;
                        @args.append:
                            "$name",
                        {c2p_parse_dynamic($p, $pptype)}
                        EOPC
                    $p{$name}.p2c_code = qq:to<EOPI>;
                        # TODO: dwimmery
                        @bufs.append: \$\!$name.bufs;
                        EOPI
                }
                else {
                    $p{$name}.c_offset = $offset;
                    $p{$name}.c_attr = qq:to<EOCT>;
                        # TODO: need to verify correct packing
                        HAS {$pptype eq "Event" ?? "X11::XCB::Event" !! $pptype}::cstruct \$.$name$rw;
                        # This only works right on objects gotten via nativecast and not full of zeros
                        method {$name}___pointerto \{
                            nativecast(Pointer[uint8],\$\!$name);
                        }
                        EOCT
                    $p{$name}.c2p_arg = qq:to<EOPA>;
                        :{$name}({$pptype}.new(nativecast(Pointer[uint8],\$\!$name),
                                   :left({$pptype}::cstruct.wiresize), :!free))
                        EOPA
                    $p{$name}.p2c_init = qq:to<EOPI>;
                        \{
                            # Workaround until binding/assigning to HAS attributes is smoothed out
                            my \$buf := nativecast(Pointer[uint8], self);
                            my \$offset = do given CArray[uint8].new(42 xx nativesizeof(self)) \{
                                nativecast(::?CLASS, \$_).{$name}___pointerto - nativecast(Pointer[uint8], \$_)
                            }
                            \$buf := Pointer[uint8].new(\$buf + \$offset);
                            \$buf := nativecast(CArray[uint8], \$buf);
                            my \$cs := do given \$p6.$name.WHO<cstruct>.new \{ .nativize(\$p6.$name); \$_ };
                            \$buf[^nativesizeof(\$\!$name)] = nativecast(CArray[uint8],\$cs)[^nativesizeof(\$\!$name)];
                        }
                        EOPI
#                        self.^attributes.first(*.name eq '\$\!$name').set_value(self,do given {$pptype}::cstruct.new \{ .nativize(\$p6.$name), \$_ });
                }
            }

            else {
                note "Expected align mod {NCsizetype($type)}, got $offset for $name"
                    if $offset % NCsizetype($type);
                $offset += NCsizetype($type);

                $p{$name}.c_attr ~= qq:to<EOCA>;

                constant {$name}___maxof =
                    2 ** (wiresize({NCtype($type)}) * 8) - 1;
                EOCA
                if $dynsize {
                    $p{$name}.c2p_code = qq:to<EOPC>;
                        @args.append:
                            "$name",
                        {c2p_parse_dynamic($p, $type)}
                        EOPC
                }
            }
            if $f.name eq "exprfield" { # maybe use %quirks for this
                # Fake it.  There is only one of these in the whole batch.
                $p<odd_length>.p2c_init = '$!odd_length = +@.string +& 1;';
                $p<odd_length>.p_attr = |();
                $p<odd_length>.c2p_arg = |();
                $p<odd_length>.c2p_code =
                    "# XXX need to use \$odd_length to decide whether last CHAR2B is half";
            }
            $p{$name}.c_offset = $offset;
        }
        when "fd" {
            $p{$name}.p_attr = "has \$.$name is rw;";
            $p{$name}.fd_init = '$.' ~ $name ~ ' = $fdb[$i]; $i++;';
            $p{$name}.fd_send = 'xcb_send_fd($c.xcb, $.' ~ $name ~ ');';
        }
        when "list" {
            if $name ~~ /^alignment_pad/ {
                # Not really a list.  A complicated XML mess of a formula
                # for how much padding is needed to keep C-like alignment.
                # Since the protocol is relatively consistent about alignment,
                # we can just as easily deduce that.
                #
                # Newer XCBXML has less of these as they added an alignment
                # checker as well.
                #
                # Send it down to the padding case
                proceed;
            }
            if not +$f.elements or
                $f.elements».name.join(" ") ne any <fieldref value> {
                $dynsize = True;
                my $eq;

                if $f.elements == 1 and +$f.elements(:TAG<popcount>) {
		    if +$f.elements[0].elements == 1 and
                       +$f.elements[0].elements(:TAG<fieldref>) {
                        MakeSparseArray($p, $name, $type,
                                        $f.elements[0].elements[0])
                    }
                    else {
                        $p{$name}.p_attr = "# TODOP6: unknown popcount formulation";
                    }
                    succeed
                }
                elsif +$f.elements {
                    $eq = build_equation($f);
                }
                else {
                    # Just run until length is exhausted.
                    # This will need to be adjusted for widths.
                    $eq = '($pstruct.length * 4 - $oleft + $left)';
                }

                if $type eq "void" and $eq ~~ /pstruct\.(\w+)\s\*<-alpha>*\$pstruct.format.*div\s8/ {
                    # Typical Properties handling, map to typed array
                    my $cf = $/[0].Str;
                    $p<format>.p_attr = |();
                    $p<format>.p2c_init = "\$\!format = nativesizeof(\$p6.$name.of) * 8;";
                    $p<format>.c2p_arg = |();
                    $p<format>.c2p_code = |();
                    $p<format>.p2c_code = |();
                    $p{$cf}.p_attr = |();
                    $p{$cf}.p2c_init = qq:to<EOPC>;
                        die 'length exceeds field $cf' if \$p6.$name.elems > {$cf}___maxof;
                        \$\!$cf = \$p6.$name.elems;
                        EOPC
                    $p{$cf}.c2p_arg = |();
                    $p{$cf}.c2p_code = |();
                    $p{$cf}.p2c_code = |();
                    $p{$name}.p2c_code = "\@bufs.push(Buf[\$.$name.of].new(\$.$name.values));";
                    $p{$name}.p_attr = "has \$\.$name is rw; # XXX Should be @ but has problems";
                    $p{$name}.c2p_code = qq:to<EOCC>;
                        die "Short Packet" if \$left < $eq;
                        \{
                            my \$t = do given \$pstruct.format \{
                                when 8 \{ uint8 }
                                when 16 \{ uint16 }
                                when 32 \{ uint32 }
                                default \{ die "Bad format value" }
                            };
                            @args.append:
                                "$name",
                                array[\$t].new(nativecast(CArray[\$t],Pointer.new(\$p + \$oleft - \$left))[^\$pstruct.$cf]);
                        }
                        \$left -= $eq;
                        EOCC
                    succeed
                }
                if ($eq ~~ /TODOP6/) {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 complicated $_ ($eq) of {$f.attribs<type>}";
                    $p{$TODOP6}.p_attr = "# $TODOP6 complicated $_ ($eq) of {$f.attribs<type>}";
                }
                elsif $type eq any <char STRING8> {
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
                elsif %cstructs{$type} -> $pt {
                    if $eq ~~ /pstruct\.<!before length<!alpha>>/ {
                        if %quirks{"list:$name"}:exists and %quirks{"list:$name"}($p, $f, :$padnum) {
                            succeed
                        }
                        else {
                            $TODOP6++;
                            $p{$name}.c_attr = "# Dynamic layout: {$type}s $TODOP6 fields other than .length.";
                        }
                    } else {
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    }
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push(\$_.bufs) for \$.$name;
                        EOCC
                    my $loop = +$f.elements ?? "for 0..^$eq" !! 'while $left';
                    $p{$name}.c2p_code = qq:to<EOPC>;

                    @args.append:
                        "$name",
                        ($loop \{
                                # XXX no assurance pointer math will not start adding by sizeof
                                $pt\.new(Pointer.new(\$p + \$oleft - \$left),
                                         :\$left, :!free);
                            }
                        );
                    EOPC
                }
                elsif NCtype($f.attribs<type>) eq any <
                    int8 int16 int32 int64 uint8 uint16 uint32 uint64 long
                    longlong bool size_t num32 num64
                > {
                    my $nct = NCtype($f.attribs<type>);
                    if $eq ~~ /pstruct\.<!before length<!alpha>>/ {
                        if $eq ~~ /\(\$pstruct\.(<alpha>+)\s\*\s[\$pstruct\.(<alpha>+)|(\d+)]\)/ {
                            # multidim array
                            (my $f1, my $f2) = |($/[0,1])».Str;
                            if $f1 !~~ /^\d+$/ {
                                $p{$f1}.p2c_init = "\$\!$f1 = \$p6\.$name.shape[0];";
                                $p{$f1}.p_attr = |();
                                $p{$f1}.c2p_arg = |();
                            }
                            if $f2 !~~ /^\d+$/ {
                                $p{$f2}.p2c_init = "\$\!$f2 = \$p6\.$name.shape[1];";
                                $p{$f2}.p_attr = |();
                                $p{$f2}.c2p_arg = |();
                            }
                            $p{$name}.p2c_code = qq:to<EOCC>;
                                    \@bufs.push(Buf[$nct].new(|\@.$name));
                                EOCC
                            $p{$name}.c2p_code = qq:to<EOPC>;
                                die("Short Packet") if \$left < (($eq) * wiresize($nct));
                                @args.append:
                                    "$name",
                                    ((nativecast(CArray[$nct],Pointer.new(\$p + \$oleft - \$left))[
                                         (0..^$eq)])[(0..^$eq).rotor(\$pstruct.$f1)]
                                    );
                                EOPC
                            succeed;
                        }

                        $TODOP6++;
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s $TODOP6 fields other than .length $eq";
                    } else {
                        $p{$name}.c_attr = "# Dynamic layout: {$type}s";
                    }
                    $eq ~= "div wiresize($nct)" unless +$f.elements;
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC>;
                        \@bufs.push(Buf[$nct].new(|\@.$name));
                        EOCC
                    $p{$name}.c2p_code = qq:to<EOPC>;
                    die("Short Packet") if \$left < (($eq) * wiresize($nct));
                    @args.append:
                        "$name",
                        (nativecast(CArray[$nct],Pointer.new(\$p + \$oleft - \$left))[0..^($eq)]
                        );
                    EOPC
                }
                else {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 NYI $_";
                    $p{$TODOP6}.p_attr = "# $TODOP6 NYI $_";
                }
            }
            elsif $f.elements(:TAG<value>) and $type eq "char" and $name eq "event"  {
                # Special case SendEvent
                $p<event>.p_attr = q:to<EOPA>;
                    has $.event;
                    EOPA
                $p<event>.p2c_code = q:to<EOSP>;
                    if $.event ~~ Buf {
                        die('As a buffer $.event must have 32 bytes exactly')
                            unless $.event.bytes == 32;
                        @bufs.push($.event);
                    }
                    else {
                        die('Should have an Event or a Buf for $.event')
                            unless $.event ~~ Buf|Event;
                        my @bytes = |(|nativecast(CArray[uint8],$_)[^$_.bytes]
                                      for $.event.bufs);
                        # pad out
                        @bytes[+@bytes..31] = 0 xx *;
                        @bufs.push(Blob.new(|@bytes));
                    }
                    EOSP
                $p<event>.c2p_code = qq:to<EOSC>;
                    die "Short packet" unless \$left >= 32;
                    @args.append: "event",
                        (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[^32]
                        ).decode("utf8"));
                    \$left -= 32;
                    EOSC
            }
            elsif $f.elements(:TAG<value>) -> [ $val ] {
                my $frval = $val.contents.Str.Int;
                my $nct = NCtype($type);
                unless $dynsize {
                    # We know where the structure is exactly and it is
                    # a fixed size structure.  Until CStruct can handle
                    # shaped arrays, pad it out.
                    $p{$name}.c_attr = (for ^+$frval -> $i {
                        "has $nct \$.{$name}___pad$i;"
                    }).join("\n");
                    if $f.attribs<type> eq "char" {
                        $p{$name}.p2c_init = qq:to<EOCF>;
                            do given \$p6\.{$name}.encode('utf8') \{
                                die "String must be $frval bytes long" if .elems != $frval;
                            EOCF
                        $p{$name}.c2p_arg = ":{$name}(Buf.new(" ~
                            (for ^+$frval { "\$.{$name}___pad$_" }).join(",")
                            ~ ").decode('utf8'))";
                    }
                    else {
                        $p{$name}.p2c_init = qq:to<EOCF>;
                            do given \$p6\.{$name} \{
                                die "Array must be $frval items long" if .elems != $frval;
                            EOCF
                        $p{$name}.c2p_arg = ":{$name}(" ~
                            (for ^+$frval { "self.{$name}___pad$_" }).join(",")
                            ~ ")";
                    }
                    $p{$name}.p2c_init ~= (for ^+$frval -> $i {
                        "    \$\!{$name}___pad$i = \$_[$i];"
                    }).join("\n");
                    $p{$name}.p2c_init ~= "\n}";
                }
                if $f.attribs<type> eq "char" {
                    $p{$name}.c_attr ||= "# Dynamic layout: $name\[$frval] chars";
                    $p{$name}.p_attr = "has \$.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC> if $dynsize;
                    given \$\.{$name}.encode('utf8') \{
                        die "String must be $frval bytes" if .elems != $frval;
                        \@bufs.push(Blob.new(\$_.values)
                    }
                    EOCC
                    $p{$name}.c2p_code = qq:to<EOPC> if $dynsize;
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
                    longlong bool size_t num32 num64
                > {
                    $p{$name}.c_attr ||=
                        "# Dynamic layout: $name\[$frval] of $type";
                    $p{$name}.p_attr = "has \@.$name is rw;";
                    $p{$name}.p2c_code = qq:to<EOCC> if $dynsize;
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
                $dynsize = True;
                my $frname = $fr.contents.Str;
                if $f.attribs<type> eq "STRING8" or
                    $f.attribs<type> eq "char"
                    # this is tacky -- for ImageText8
                    and !($name eq "string" and $p<x>:exists) {

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
               elsif $f.attribs<type> eq "CHAR2B" or $type eq "char" and $name eq "string" and $p<x>:exists {
                    # Font stuff.  Use a buffer not a string due to encoding

                    # Go back and remove the _len field from perl6 attributes
                    $p{$frname}.p_attr = "# $frname not needed in P6 object";

                    my $sz = $type eq "CHAR2B" ?? 2 !! 1;

                    $p{$frname}.p2c_init = qq:to<EOPC>;
                        die ("Oddly short buffer") if \$p6.{$name}.bytes % $sz;
                        my \${$frname}___sizeof = \$p6.{$name}.bytes div $sz;
                        die ("Maximum field size exceeded")
                            if \${$frname}___sizeof > {$frname}___maxof;
                        EOPC

                    my $c2p_len = $p{$frname}.c2p_code;
                    if $c2p_len ~~ s/^.*?\"$frname\"\s*\,// {
                        $p{$frname}.c2p_code = qq:to<EOC1>;
                            # No p6 attribute $frname to init but may need value in place
                            my \${$frname}___inplace = $c2p_len * $sz;
                            EOC1
                    }
                    else {
                        $c2p_len = "";
                        $p{$frname}.c2p_code = "# No p6 attribute $frname to init";
                    }
                    if $p{$frname}.c_attr ~~ /^\#/ {
                       my $lc = "self.{$name}.bytes";
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
                        given \$\.{$name} \{
                            if .elems \{ \@bufs.push(\$_) }
                        }
                        EOCC

                    $p{$name}.c2p_code = $c2p_len ?? qq:to<EOP1> !! qq:to<EOP2>;
                            die("Short packet")
                                unless \$left >= \${$frname}___inplace;
                            @args.append:
                                "$name",
                                (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[
                                    ^\${$frname}___inplace
                                ]));
                            \$left -= \${$frname}___inplace;
                            EOP1
                            die("Short packet")
                                unless \$left >= \$pstruct\.$frname;
                            @args.append:
                                "$name",
                                (Buf.new(nativecast(CArray[uint8], Pointer.new(\$p + \$oleft - \$left))[
                                    ^\$pstruct\.$frname
                                ]));
                            \$left -= \$pstruct\.$frname;
                            EOP2
                }
                elsif NCtype($f.attribs<type>) eq any <
                    int8 int16 int32 int64 uint8 uint16 uint32 uint64 long
                    longlong bool size_t num32 num64
                > {
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
                                die "Short Packet" unless \$left >= wiresize($nct);
                                NEXT \{ \$left -= wiresize($nct) };
                                nativecast(Pointer[{$nct}],Pointer.new(\$p + \$oleft - \$left)).deref;
                            });
                        EOP1
                        @args.append:
                            "$name",
                            (for 0..^\$pstruct.$frname \{
                                    # XXX no assurance pointer math will not start adding by sizeof
                                die "Short Packet" unless \$left >= wiresize($nct);
                                NEXT \{ \$left -= wiresize($nct) };
                                nativecast(Pointer[{$nct}],Pointer.new(\$p + \$oleft - \$left)).deref;
                            });
                        EOP2
                }
                elsif $type eq "STR" { # Map "String" directly to Perl6 Str

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
                    $p{$TODOP6}.c_attr = "# $TODOP6 unknown $_";
                    $p{$TODOP6}.p_attr = "# $TODOP6 unknown $_";
                }
            }
        }
        when "valueparam" {
            $dynsize = True;
            # Replaced by "switch" but some people may be working
            # off older xml files.

            # One of the deficiencies it had was not listing the
            # enum of the mask bit values.  So we fix this up.
            my %vpmap = (
                :CreateWindow<CW>, :ChangeWindowAttributes<CW>,
                :SetAttributes<xproto:CW>, :ConfigureWindow<ConfigWindow>,
                :CreateGC<GC>, :ChangeGC<GC>, :ChangeKeyboardControl<KB>,
                :PrintInputSelected<EvMask>, :PrintSelectInput<EvMask>,
                :CreatePicture<CP>, :ChangePicture<CP>
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
            my $renum = mod.renum($f, $enum);
            my $type = $f.attribs<value-mask-type>;
            my $name = $f.attribs<value-mask-name>;
            my $pname = $f.attribs<value-list-name>;
            $p{$name}.c_attr = qq:to<EOCT>;
                has {NCtype($type)} \$.$name is rw;
                constant {$name}___maxof =
                    2 ** (wiresize({NCtype($type)}) * 8) - 1;
                # Dynamic layout -- bit enabled fields
                EOCT
            $p{$name}.p_attr = qq:to<EOPT>;
                # Perl6 object does not need attribute for $name
                constant {$name}___maxof =
                    2 ** (wiresize({NCtype($type)}) * 8) - 1;
                has Any \%.$pname\{{$renum}Enum\::{$renum}} is rw;
                EOPT
            $p{$name}.p2c_code = qq:to<EOPC>;
                \{
                    my \$b;
                    loop (\$b = 1; \$b < {$name}___maxof; \$b +<= 1) \{
                        last if \%.$pname\{{$renum}Enum\::{$renum}(\$b)}:exists;
                    }
                    if \$b < {$name}___maxof \{
                        @bufs.push: Buf[uint32].new(
                            (loop (\$b = 1; \$b < {$name}___maxof; \$b +<= 1) \{
                                if \%.$pname\{{$renum}Enum\::{$renum}(\$b)}:exists \{
                                    (+\%.$pname\{{$renum}Enum\::{$renum}(\$b)}) +& 0xffffffff;
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
                            \$\!{$name} +|= \$b if \$p6.$pname\{{$renum}Enum\::{$renum}(\$b)}:exists;
                            \$b +<= 1;
                    };
                }
                EOPI
            #TODO: C/binary to perl object creation

        }
        when "switch" {
            $dynsize = True;
            my $pname = $name;
            my @names = ($f.elements(:FIRST).name eq "fieldref" ?? $f.elements(:FIRST).contents.Str
                !! (.contents.Str for $f.elements(:FIRST).elements(:TAG<fieldref>, :RECURSE)));
            my $parent = $f.parent;
            my @types = ($parent.elements(:name($_))[0].attribs<type> for @names);
            my $enum = $f.elements(:TAG<bitcase>)[0].elements(:TAG<enumref>)[0].attribs<ref>;

            my $renum = mod.renum($f, $enum);
            my %nots;
            my $formula;
            my $formula_c;
            my @cases;

            # Only handle formulas like fee & (fie & (~foo & ~fum)) for now.
            # (Really, we are doing this all for one particular request, but hey,
            #  maybe it will be useful for a future or unpublished extension)
	    if +@names > 1 {
                if $f.elements(:FIRST).name eq "op" {
                    my multi sub formulize ($node where { $_.name eq "op" }) {
                        my constant %optrans := {'&' => ' +& ', '|' => ' +| '};
                        [~] "( ", (formulize($_) for $node.elements).join(%optrans{$node.attribs<op>}), " )";
                    }
                    my multi sub formulize ($node where { $_.name eq "unop" and $_.attribs<op> eq "~" }) {
                        [~] "( {$node.elements[0].contents.Str}___maxof +^ ", |formulize($node.elements[0]), " )";
                    }
                    my multi sub formulize ($node where { $_.name eq "fieldref" }) {
                        [~] '([+|] %.', $node.contents.Str, '.keys)';
                    }

                    my multi sub formulize_c ($node where { $_.name eq "op" }) {
                        my constant %optrans := {'&' => ' +& ', '|' => ' +| '};
                        [~] "( ", (formulize_c($_) for $node.elements).join(%optrans{$node.attribs<op>}), " )";
                    }
                    my multi sub formulize_c ($node where { $_.name eq "unop" and $_.attribs<op> eq "~" }) {
                        [~] "( {$node.elements[0].contents.Str}___maxof +^ ", |formulize_c($node.elements[0]), " )";
                    }
                    my multi sub formulize_c ($node where { $_.name eq "fieldref" }) {
                        [~] '$pstruct.', $node.contents.Str;
                    }
                    for $f.elements(:FIRST).elements(:TAG<op>) {
                        if $_.attribs<op> ne '&' {
                            $TODOP6++;
                            $p{$TODOP6}.c_attr = "# $TODOP6 unhandled switch op {$_.attribs<op>} $pname/{@names} of {@types} from {$enum}/{$renum}";
                            $p{$TODOP6}.p_attr = "# $TODOP6 unhandled switch op {$_.attribs<op>} $pname/{@names} of {@types} from {$enum}/{$renum}";
                            succeed;
                        }
                    }
                    for $f.elements(:FIRST).elements(:TAG<unop>) {
                        if $_.attribs<op> ne '~' or $_.elements(:FIRST).name ne "fieldref" {
                            $TODOP6++;
                            $p{$TODOP6}.c_attr = "# $TODOP6 unhandled switch op {$_.attribs<op>} $pname/{@names} of {@types} from {$enum}/{$renum}";
                            $p{$TODOP6}.p_attr = "# $TODOP6 unhandled switch op {$_.attribs<op>} $pname/{@names} of {@types} from {$enum}/{$renum}";
                            succeed;
                        }
                    }
                    $formula = formulize($f.elements(:FIRST));
                    $formula_c = formulize_c($f.elements(:FIRST));
                }
                else {
                    $TODOP6++;
                    $p{$TODOP6}.c_attr = "# $TODOP6 unknown switch syntax $pname/{@names} of {@types} from {$enum}/{$renum}";
                    $p{$TODOP6}.p_attr = "# $TODOP6 unknown switch syntax $pname/{@names} of {@types} from {$enum}/{$renum}";
                    succeed;
                }
            }
            else {
                $formula = [~] ' ([+|] %.', @names[0], '.keys)';
                $formula_c = '$pstruct.' ~ @names[0];
            }
	    for @names -> $name {
                my $unop = $f.elements(:TAG<fieldref>, :RECURSE).first({$_.contents.Str eq $name}).parent;
                %nots{$name} = (so ($unop.name eq "unop") and ($unop.attribs<op> eq '~'));
            }

            for @names Z @types -> ($name is copy, $type) {
                my $not = %nots{$name};
                my $note = $not
                ?? "# Dynamic layout -- bit enabled fields"
                !! "# Dynamic layout -- bits here disable enabled fields";
                $p{$name}.c_attr = qq:to<EOCT>;
                     has {NCtype($type)} \$.$name is rw;
                     constant {$name}___maxof =
                         2 ** (wiresize({NCtype($type)}) * 8) - 1;
                     $note
                     EOCT
                $p{$name}.p2c_init = qq:to<EOPI>;
                    \$\!{$name} = 0;
                    \{
                        my \$b = 1;
                        while \$b < {$name}___maxof \{
                            \$\!{$name} +|= \$b if \$p6.$name\{{$renum}Enum\::{$renum}(\$b)}:exists;
                            \$b +<= 1;
                        };
                    }
                    EOPI
                if $not {
                    $p{$name}.p_attr = qq:to<EOPT>;
                    constant {$name}___maxof =
                        2 ** (wiresize({NCtype($type)}) * 8) - 1;
                    # We would like to use a parameterized SetHash here.
                    has Bool \%.$name\{{$renum}Enum\::{$renum}} is rw;
                    EOPT
                }
                else {
                    $p{$name}.p_attr = qq:to<EOPT>;
                    constant {$name}___maxof =
                        2 ** (wiresize({NCtype($type)}) * 8) - 1;
                    has \%.$name\{{$renum}Enum\::{$renum}} is rw;
                    EOPT
                }
            }

            # Now build the list of optional fields
	    my $cases = bitswitch.new(:xml($f));
            MakeCases($cases, params.new(:parent($p)));
            $p{$pname}.p_subclasses = $cases.p6classes;

            $p{@names[0]}.p2c_code = qq:to<EOPC>;
                \{
                    my constant \$formbits = (1 +< (max ({@types.map({NCtype($_)}).join(", ")}).map: \{ 8 * wiresize(\$_) })) - 1;
                    my \$f = $formula;

                    my \$b;
                    loop (\$b = 1; \$b < \$formbits; \$b +<= 1) \{
                        next unless \$b +& \$f;
                        for @names.grep({not %nots{$_}}).map({'$%.' ~ $_}).join(", ") \{
                            if \$_\{{$renum}Enum\::{$renum}(\$b)}:exists \{
                                die "Must be of type \{self.{$pname}_typemap\{\{{$renum}Enum\::{$renum}(\$b)}}.^name}"
                                    unless (\$_\{{$renum}Enum\::{$renum}(\$b)}.isa(
                                       self.{$pname}_typemap\{{$renum}Enum\::{$renum}(\$b)})
                                    );
                                @bufs.append: \$_\{{$renum}Enum\::{$renum}(\$b)}.bufs;
                                last;
                            }
                        }
                    }
                }
                EOPC


            # TODO: multiple present fields... but nothing uses them
            $p{@names[0]}.c2p_code = qq:to<EOPC>;
                \{
                    my constant \$formbits = (1 +< (max ({@types.map({NCtype($_)}).join(", ")}).map: \{ 8 * wiresize(\$_) })) - 1;
                    my \$f = $formula_c;

                    my \$b;
                    @args.append: "{@names[0]}", Hash[Any,Any].new(
                        (loop (\$b = 1; \$b < \$formbits; \$b +<= 1) \{
                            next unless \$b +& \$f;
                            \$_\{{$renum}Enum\::{$renum}(\$b)} =>
                                \$pstruct.{$pname}_typemap\{{$renum}Enum\::{$renum}(\$b)}.new(
                                    Pointer.new(\$p + \$oleft - \$left), :\$left, :!free
                                )
                        }))
                }
                EOPC
        }
        when "doc" | "reply" {
            # Handled elsewhere
            succeed;
        }
        when "pad" | "list" { # "list" is proceeding from above
            unless $dynsize {

                my $offset = $p.params.elems ?? $p.params[*-1].c_offset !! $p.init_offset;

                # Embedded pad inside static structure
                if not $f.isa(XML::Comment) and $f.attribs<bytes>:exists {
                    # Explicit size
                    $padnum++;
                    (for 0..^$f.attribs<bytes> {
                        given "pad{$padnum}_$_" {
                            $p{$_}.c_offset = ++$offset;
                            $p{$_}.c_attr = "has uint8 \$.$_;";
                            $p{$_}.p_attr = "# padding here in CStruct";
                        }
                    }).join(" ");
                    succeed;
                }
                # <pad align=X>, or an alignment_pad "list", or a "pad" comment
                # from before alignment markup.  Reckon it from structure.
                # Note c_offset does not necessarily increase by the actual
                # size of its structure member, maybe the size mod 8.
                my $align = c_alignment_rules($p);
                if not $f.isa(XML::Comment) and $f.attribs<align>:exists {
                    $align = $f.attribs<align>
                }
                $padnum++;
                my $off = $offset;
                while ($off % $align) {
                    given "pad{$padnum}_{$align - $_ - 1}" {
                       $p{$_}.c_offset = ++$offset;
                       $p{$_}.c_attr = "has uint8 \$.$_;";
                       $p{$_}.p_attr = "# padding here in CStruct";
                    }
                    $off--;
                }
                succeed;
            }
            # Pad in dynamic part of packet.
            $padnum++;
            my $align = '$.cstruct.calign';
            if not $f.isa(XML::Comment) and $f.attribs<align>:exists {
                $align = $f.attribs<align>
            }
            $p{"pad$padnum"}.p2c_code = qq:to<EOCP>;
                # TODO: we are assuming everything before us is aligned here
                \@bufs.push(padbuf(([-] 0, |(.bytes for \@bufs)) % \$.cstruct.calign));
                EOCP
            $p{"pad$padnum"}.c2p_code = qq:to<EOPC>;
                \{
                    my \$align = $align;
                    my \$newp = nativecast(Pointer[uint8], \$p);
                    my \$oldp = nativecast(Pointer[uint8], \$pstruct);

                    \$left -= (\$oldp - \$newp - \$oleft + \$left) % \$align;
                    die("Short packet") unless \$left >= 0;
                }
                EOPC
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
        my $dynsize = 0;
        %errorcopies{$oname ~ $error.attribs<name>.Str} := $p;
        for $error.nodes -> $e {
            MakeCStructField($p, $e, $padnum, $dynsize)
                if $e.isa(XML::Element)
                or $e.isa(XML::Comment) and $e.data ~~ /:i pad/
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
            $p := %errorcopies{$oname ~ $error.attribs<ref>.Str} //
                  %errorcopies{$error.attribs<ref>.Str};
        }
        else {
            $p := %errorcopies{$oname ~ $error.attribs<name>.Str};
        }
        my $number = $error.attribs<number>;

        @cstructs.push(qq:to<EOCS>);

                our class cstruct does cpacking is repr("CStruct") \{

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
                    method nativize(\$p6) \{
                        \$\!sequence = \$p6.sequence // 0;
            {$p.params».p2c_init.join("\n").indent(12)}
                    }
                };
                my \$.cstruct = cstruct;

            EOCS

        my $clname = $error.attribs<name>.Str;
        $clname = $clname.lc.tc if $clname ~~ /^<upper>+$/;
        $mod.errors{$number} = $oname ~ $clname ~ "Error";
        %cstructs{$oname ~ $error.attribs<name>} = $clname;
        %cstructs{$clname} = $clname;

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

            { makemeth_child_bufs($p, :indent(4)) }

            { makemeth_child_structs($p, :indent(4)) }

            $mcode

            }
            EO6C
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}

my %eventcopies;
sub MakeEvents($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    for $mod.xml.root.elements(:TAG<event>) -> $event {
        my $xge = $event.attribs<xge>:exists
                  and $event.attribs<xge>:exists ~~ /:i true/;
        my params $p .= new(:init_offset($xge ?? 14 !! 3));
        my $padnum = -1;
        my $dynsize = 0;

        %eventcopies{$oname ~ $event.attribs<name>.Str} := $p;
        for $event.nodes -> $e {
            MakeCStructField($p, $e, $padnum, $dynsize)
                if $e.isa(XML::Element)
                or $e.isa(XML::Comment) and $e.data ~~ /:i pad/
        }
        for $event.elements -> $e {
            if $event.elements(:TAG<doc>) -> [ $doc ] {
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

sub MakeEvents2($mod) {
    my @cstructs;
    my @p6classes;
    my $oname = $mod.cname eq "xproto" ?? "" !! $mod.modname;

    unless $oname {
        # Add GeGeneric by hand since the xml is pseudo
        @cstructs.push(q:to<EGEC>);
            our class cstruct does cpacking is repr("CStruct") {
                has uint8 $.response_type is rw;
                has uint8 $.extension is rw;
                has uint16 $.sequence is rw;
                has uint32 $.length is rw;
                has uint16 $.getype is rw;

                method Hash {
                  { :$!response_type, :$!extension,
                    :$!sequence, :$!length, :$!getype }
                }
                method nativize($p6) {
                    :$!response_type = $p6.response_type;
                    :$!extension = $p6.extension;
                    :$!sequence = $p6.sequence;
                    :$!length = $p6.length;
                    :$!getype = $p6.getype;
                }
            }
            my $.cstruct = cstruct;
            EGEC
        @p6classes.push(qq:to<EGEP>);
            our class GeGenericEvent does Event[35] is export(:DEFAULT, :events) \{
            { @cstructs[*-1].indent(4) }
                has \$\.response_type;
                has \$\.extension;
                has \$\.sequence;
                has \$\.length;
                has \$\.getype;
            }
            EGEP
    }
    for (|$mod.xml.root.elements(:TAG<event>),|$mod.xml.root.elements(:TAG<eventcopy>)) -> $event {
        my params $p;
        my $padnum = -1;
        my $ename = $event.attribs<name>;
        next if $ename eq "GeGeneric";
        my $prestruct = "";
        my $xge = $event.attribs<xge>:exists
                  and $event.attribs<xge>:exists ~~ /:i true/;

        if $event.name eq "eventcopy" {
            $p := %eventcopies{$oname ~ $event.attribs<ref>.Str} //
                %eventcopies{$event.attribs<ref>.Str};
        }
        else {
            $p := %eventcopies{$oname ~ $ename};
        }
        my $number = $event.attribs<number>;

        if $mod.occludes.first(*.envelope eq $ename ~ "Event") -> $o {
            $o.fixparams($p);
        }

        my @HASoccludes = $p.params.map:
            { if $_.c_attr ~~ /HAS.*?<ident>\:\:cstruct/ { $/<ident>.Str } };
        if +@HASoccludes {
            my @pendoccludes = $mod.occludes.grep(!*.cstruct_done).map(*.class);
	    if @HASoccludes (&) @pendoccludes -> $got {
                for $got.keys -> $do {
                    $prestruct ~= $mod.occludes.first(*.class eq $do).cstruct;
                }
            }
        }

        my $lift = 'has uint8 $.event_code is rw;';
        my $start = 1;
        my $codes_init = '$!response_type = $p6.event_code;';
        if $p.params[0].c_attr !~~ /pad0/ {
            $lift = $p.params[0].c_doc ~ "\n" ~ $p.params[0].c_attr;
        }
        elsif $p.params[0].c_attr ~~ /___pad/ {
            $start = 0;
        }
        $lift ~= "\nhas uint16 \$.sequence is rw;";

        if $xge {
            $lift = q:to<EOGF>;
                has uint8 $.extension is rw;
                has uint16 $.sequence is rw;
                has uint32 $.length is rw;
                has uint16 $.getype is rw;
                EOGF
            $start = 0;
	    $codes_init = q:to<EOCI>
                $!response_type = 35;
                $!getype = $p6.event_code;
                EOCI
        }

        @cstructs.push(qq:to<EOCS>);

                our class cstruct does cpacking is repr("CStruct") \{

                    has uint8 \$.response_type is rw;
            $lift.indent(8)
            {({ |(.c_doc, .c_attr) } for $p.params[$start..^*]).join("\n").indent(8)}

                    method Hash \{
                        \{
                            :sequence(\$\!sequence),
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativize(\$p6) \{
                        $codes_init
                        \$\!sequence = \$p6.sequence // 0;
            {$p.params».p2c_init.join("\n").indent(12)}
                    }

                };
                my \$.cstruct = cstruct;

            EOCS

        my $clname = $event.attribs<name>.Str;
        $clname = $clname.lc.tc if $clname ~~ /^<upper>+$/;

        my $role;
	if $xge {
            $role = "XGEvent";
            $mod.xge{$number} = $oname ~ $clname ~ $role;
        }
        else {
            $role = "Event";
            $mod.events{$number} = $oname ~ $clname ~ $role;
        }
        %cstructs{$oname ~ $event.attribs<name>} = $clname;
        %cstructs{$clname} = $clname;

        my @doc;
        @doc.append(MakeClassDocs($event, $clname));


        @p6classes.push(qq:to<EO6C>);
            $prestruct
            {@doc.join("\n")}
            our class {$oname}{$clname}{$role} does $role\[$number] is export(:DEFAULT, :events) \{
                my \$.event_code = $number; # without the extension base number

                { @cstructs[*-1] }

                has \$.sequence is rw;
            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

            { makemeth_child_bufs($p, :indent(4)) }

            { makemeth_child_structs($p, :indent(4)) }

            }
            EO6C
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}

# Temporary, to cull TODOP6s
my %GoodReqs = :Nil(1);

sub MakeCases($bitswitch, $pp) {
    my @cstructs;
    my @p6classes;
    my @casemap;
    my $pname = $bitswitch.xml.attribs<name>;

    for $bitswitch.xml.elements(:TAG<bitcase>) -> $bitcase {
        my params $p .= new(:parent($pp));
        my $cstruct = "";
        my @p6fields;
        my $p6assigns = "";
        my $p6args = "";
        my $padnum = -1;
        my @reqfields;
        my $dynsize = 0;
        my $roles = " does MonoStruct";
        my $enumref = $bitcase.elements(:TAG<enumref>)[0];

        for $bitcase.elements -> $e {
            next if $e.name eq "enumref";
            MakeCStructField($p, $e, $padnum, $dynsize);
        }
        for $bitcase.elements -> $e {
            next if $e.name eq "enumref";
            if $bitcase.elements(:TAG<doc>) -> [ $doc ] {
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

        my @has = $p.params».c_attr;
        @cstructs.push: @has.first(/:i^^\s*has\s/) ?? qq:to<EOCS>

                our class cstruct does cpacking is repr("CStruct") \{

            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                    method Hash \{
                        \{
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativize(\$p6) \{
            {$p.params».p2c_init.join("\n").indent(12)}
                    }

                };
                my \$.cstruct = cstruct;
            EOCS
            !! qq:to<EONC>;
            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}
                my \$.cstruct = cpacking;
                method bufs \{
                    |self.child_bufs;
                }
            EONC

        my $enum = $enumref.attribs<ref>;
        my $renum = mod.renum($enumref, $enum);
        my $renumv = mod.renumv($enumref, $enum, $enumref.contents.Str);
        my $ename = $renum ~ "Enum::" ~ $renumv;
        my $clname = $pname ~ "::" ~ $renum ~ "::" ~ $renumv;

        my @doc;
        @doc.append(MakeClassDocs($bitcase, $clname));

        my $optnew = $p.params».c2p_code.elems ?? "" !! qq:to<EO6O>;

                # Optimize leaf nodes, since there can be tens of thousands
                multi method new (Pointer \$p, Int :\$left! is rw, Bool :\$free = True) \{
                    my \$cs = nativecast(cstruct, \$p);
                    \$left -= cstruct.wiresize;
                    fail("Short packet.") unless \$left >= 0;
                    my \$res = self.bless(|\$cs.Hash);
                    xcb_free \$p if \$free;
                    \$res;
                }
            EO6O

        @casemap.push: "$ename => $clname";

        @p6classes.push(qq:to<EO6C>);
            {@doc.join("\n")}
            our class {$clname} does Struct$roles \{

                { @cstructs[*-1] }

            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

            { makemeth_child_bufs($p, :indent(4)) }

            { makemeth_child_structs($p, :indent(4)) }

            $optnew

            }
            EO6C
    }
    $bitswitch.cstructs.append(@cstructs);
    @p6classes.push(qq:to<EOCH>);
        my \%.{$pname}_typemap := :\{
        { @casemap.join(",\n").indent(4) }
        }
        EOCH
    $bitswitch.p6classes.append(@p6classes);
}

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
        my $dynsize = 0;
        my $roles = " does MonoStruct";
        my $morphstructor = ""; # Replace normal .new if a generic
	my $prestruct = "";

        for $struct.nodes -> $e {
            MakeCStructField($p, $e, $padnum, $dynsize)
                if $e.isa(XML::Element)
                or $e.isa(XML::Comment) and $e.data ~~ /:i pad/
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
        my @HASoccludes = $p.params.map:
            { if $_.c_attr ~~ /HAS.*?<ident>\:\:cstruct/ { $/<ident>.Str } };
        if +@HASoccludes {
            my @pendoccludes = $mod.occludes.grep(!*.cstruct_done).map(*.class);
	    if @HASoccludes (&) @pendoccludes -> $got {
                for $got.keys -> $do {
                    $prestruct ~= $mod.occludes.first(*.class eq $do).cstruct;
                }
            }
        }

	if $mod.occludes.first(*.envelope eq $struct.attribs<name>) -> $o {
            $o.fixparams($p);
        }
	if $mod.occludes.first(*.generic eq $struct.attribs<name>) -> $o {
            $morphstructor = $o.morphstructor;
        }

        my $clname = $struct.attribs<name>.Str;
        $clname = $clname.lc.tc if $clname ~~ /^<upper>+$/;
        $clname = (given $clname {
            when "Str" { "String" }
            when "Modeinfo" { "{$oname}Modeinfo" }
            when "Format" { "{$oname}Format" }
            when "Event" { "{$oname}Event" }
            when "INT64" { "Counter64" }
            default    { $_  }
        });
        my $n = $struct.attribs<name>;
        %cstructs{$n} = $clname;
        %cstructs{$clname} = $clname;
        while %cstructs{$n}:exists {
            %embeddable{$n} = True unless $dynsize;
            last if $n eq %cstructs{$n} or $++ > 20;
	    $n = %cstructs{$n};
        }

        my sub resolvesubclass ($clname is copy) {
            return "" unless $mod.subclasses{$clname};
            while $mod.subclasses{$clname} -> $from { $clname = $from };
	    $clname;
        }

        my @allsuchoccludes = $struct.attribs<name>,
            |$mod.subclasses.grep({ resolvesubclass($_.key) eq $struct.attribs<name> }).map(*.key);

        my $skiprest = 0;
        for @allsuchoccludes.map({ $mod.occlude{$_} // |() }) -> $o {
            once {
                my $rname = $o.isa(ocrole) ?? $o.users[0].prole_name !! $o.prole_name;
                $mod.occludes.first(*.class eq $rname).fixparams($p, :subclass($rname));
            }
            my @classes;
            if ($o.isa(ocrole)) {
                $skiprest = 1 if $o.name eq $struct.attribs<name>;
                unless $o.done {
                    @cstructs.push: qq:to<EORC>;
                        class {$o.name}::cstruct does cpacking is repr("CStruct") \{

                    {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                            method Hash \{
                               \{
                    {$p.params».c2p_arg.join(",\n").indent(16)}
                                }
                            }
                            method nativize(\$p6) \{
                    {$p.params».p2c_init.join("\n").indent(12)}
                            }
                        };
                    EORC
                    # TODO docs
                    @p6classes.push(qq:to<EOR6>);
                        role {$o.name} \{
                        {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}
                        { makemeth_child_bufs($p, :indent(4)) }
                        { makemeth_child_structs($p, :indent(4)) }
                        }
                        { @cstructs[*-1].indent(-4) }
                        EOR6
                    $o.done = True;
                }
                @classes = $o.users;
            }
            elsif $o.cname ne $struct.attribs<name> {
                @cstructs.push: qq:to<EORC>;
                        our class cstruct does cpacking is repr("CStruct") \{

                        {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                            method Hash \{
                               \{
                    {$p.params».c2p_arg.join(",\n").indent(16)}
                                }
                            }
                            method nativize(\$p6) \{
                    {$p.params».p2c_init.join("\n").indent(12)}
                            }
                        };
                        my \$.cstruct = cstruct;
                    EORC
                @p6classes.push(qq:to<EO6C>);
                    our class {$o.cname} does Struct is MonoStructClass does {$o.prole_name}\[{$o.prole_val}] is export(:DEFAULT, :structs) \{
                    { @cstructs[*-1] }
                    {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}
                    { makemeth_child_bufs($p, :indent(4)) }
                    { makemeth_child_structs($p, :indent(4)) }
                    # Optimize leaf nodes, since there can be tens of thousands
                        multi method new (Pointer \$p, Int :\$left! is rw, Bool :\$free = True) \{
                            my \$cs = nativecast(cstruct, \$p);
                            \$left -= cstruct.wiresize;
                            fail("Short packet.") unless \$left >= 0;
                            my \$res = self.bless(|\$cs.Hash);
                            xcb_free \$p if \$free;
                            \$res;
                        }
                    }
                    EO6C
            }

            for @classes -> $c {
                @p6classes.push(qq:to<EOUC>);
                    our class {$c.cname} does Struct is MonoStructClass does {$c.prole_name}\[{$c.prole_val}]{" does {$c.sharedrole}" if $c.sharedrole} is export(:DEFAULT, :structs) \{
                        constant cstruct = {$c.cstruct}::cstruct;
                        method cstruct \{ cstruct }

                        # XXX Inheriting this seems problematic
                        # Optimize leaf nodes, since there can be tens of thousands
                        multi method new (Pointer \$p, Int :\$left! is rw, Bool :\$free = True) \{
                            my \$cs = nativecast(cstruct, \$p);
                            \$left -= cstruct.wiresize;
                            fail("Short packet.") unless \$left >= 0;
                            my \$res = self.bless(|\$cs.Hash);
                            xcb_free \$p if \$free;
                           \$res;
                        }

                    }
                    EOUC
            }
        }
        if ($skiprest) {
            $mod.cstructs.append(@cstructs);
            @cstructs = ();
            $mod.p6classes.append(@p6classes);
            @p6classes = ();
            next;
        }
        if $mod.occlude{$struct.attribs<name>}.isa(ocmunge) {
            my $o = $mod.occlude{$struct.attribs<name>};
            $roles = " does {$o.prole_name}\[{$o.prole_val}]";
        }


        my @has = $p.params».c_attr;
        @cstructs.push: @has.first(/:i^^\s*has\s/) ?? qq:to<EOCS>

                our class cstruct does cpacking is repr("CStruct") \{

            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                    method Hash \{
                        \{
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativize(\$p6) \{
            {$p.params».p2c_init.join("\n").indent(12)}
                    }

                };
                my \$.cstruct = cstruct;
            EOCS
            !! qq:to<EONC>;
            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}
                my \$.cstruct = cpacking;
                method bufs \{
                    |self.child_bufs;
                }
            EONC

        my @doc;
        @doc.append(MakeClassDocs($struct, $clname));

        my $optnew = "";
        unless ($p.params».c2p_code.elems or $morphstructor) {
            $optnew = $p.params».c2p_code.elems ?? "" !! qq:to<EO6O>;
                # Optimize leaf nodes, since there can be tens of thousands
                multi method new (Pointer \$p, Int :\$left! is rw, Bool :\$free = True) \{
                    my \$cs = nativecast(cstruct, \$p);
                    \$left -= cstruct.wiresize;
                    fail("Short packet.") unless \$left >= 0;
                    my \$res = self.bless(|\$cs.Hash);
                    xcb_free \$p if \$free;
                    \$res;
                }
            EO6O
        }

	my $export = " is export(:DEFAULT, :structs)";
	with %X11::XCBquirks::StructExports{$mod.modname ~ "::" ~ $clname} {
            $export = " is export{@$_.perl}";
        }

        @p6classes.push(qq:to<EO6C>);
            $prestruct
            {@doc.join("\n")}
            our class {$clname} does Struct$roles$export \{

                {$p.params».p_subclasses.join("\n").indent(4)}

                { @cstructs[*-1] }

            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

            { $morphstructor.indent(4) }

            { makemeth_child_bufs($p, :indent(4)) }

            { makemeth_child_structs($p, :indent(4)) }

            $optnew

            }
            EO6C

        my @resolve;
        while $mod.subclasses.grep(*.value eq $clname).cache -> $list {
            for |$list {
                my $newname = $_.key;
                if not $mod.occlude{$newname} {
                    @cstructs.push(@cstructs[*-1]);
                    @p6classes.push(qq:to<EOSC>);

                    our class $newname is $clname is export(:DEFAULT, :structs) \{
                        constant cstruct = {$clname}::cstruct;
                        method cstruct \{ cstruct }
                    }

                    EOSC
                }

                @resolve.push($newname);
                $mod.subclasses{$clname}:delete;
            }
            $clname = @resolve.pop;
        }
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
        my $flagroles = "";

        my $clname = $req.attribs<name>;
        $clname = $oname ~ $clname
            if $clname eq any <
                DestroyContext QueryVersion QueryExtension ListProperties
                CreateCursor GetVersion QueryBestSize SelectInput Bell
                Enable CreateContext ChangeSaveSet GetImage PutImage
                CreatePixmap
            > or $mod.cname eq "present"
              or ($clname ~~ /^Shm/) and $mod.cname eq "xv";
        my @doc;

        next unless (for $req.elements(:TAG<reply>) -> $rep {
            my $padnum = -1;
            my @reqfields;
            my params $p .= new(:init_offset($mod.extension ?? 7 !! 7));
            my $dynsize = 0;

            state $two = 1;
            die "Two replies for request" if $two > 1;
            $two++;

            for $rep.elements -> $e {
                MakeCStructField($p, $e, $padnum, $dynsize)
                    if $e.isa(XML::Element)
                    or $e.isa(XML::Comment) and $e.data ~~ /:i pad/
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

            if $rep.elements(:TAG<fd>).elems -> $nfds {
                $flagroles ~= "does HasFD ";
                $p<nfd>.p_attr = "has \$.nfd is rw = $nfds;";
                $p<nfd>.c_attr = "has uint8 \$.nfd is rw = $nfds;";
            }
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

                    our class cstruct does cpacking is repr("CStruct") \{

                {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                        method Hash \{
                            \{
                                :sequence(\$\!sequence),
                {$p.params».c2p_arg.join(",\n").indent(16)}
                            }
                        }
                        method nativize(\$p6) \{
                            \$\!response_type = 1;
                            \$\!sequence = \$p6.sequence // 0;
                {$p.params».p2c_init.join("\n").indent(12)}
                        }

                    };
                    my \$.cstruct = cstruct;
                EOCS

            my $export = $(:DEFAULT, :replies);
            with %X11::XCBquirks::RequestExports{$mod.modname ~ "::" ~ $clname} {
                $export = $_;
            }
	    $export = "is export" ~ @$export.perl;

            @p6classes.push(qq:to<EO6C>);
                {@doc.join("\n")}
                our class {$clname}Reply
                    does Reply[OpcodeEnum::Opcode({$req.attribs<opcode>})]
                    $flagroles
                    $export \{

                {$p.params».p_subclasses.join("\n").indent(4)}

                { @cstructs[*-1] }

                    has \$.sequence is rw;
                {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

                { makemeth_child_bufs($p, :indent(4)) }

                { makemeth_child_structs($p, :indent(4)) }

                    method fd_init(\$fds, :\$nfds) \{
                        my \$fdb = nativecast(CArray[int], \$fds);
                        my \$i = 0;
                {$p.params».fd_init.join("\n").indent(8)}
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
        my params $p .= new(:init_offset($mod.extension ?? 4 !! 3));
        my $dynsize = 0;
        my $flagroles = "";
        my $fdsend = "";

        # We use the core API for this
        next if $req.attribs<name> eq "SetupAuthenticate";

        for $req.elements -> $e {
            MakeCStructField($p, $e, $padnum, $dynsize)
                if $e.isa(XML::Element)
                or $e.isa(XML::Comment) and $e.data ~~ /:i pad/
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
        if $req.elements(:TAG<fd>).elems {
            $flagroles ~= "does HasFD ";
        }
        if $mod.extension {
            $p.params.unshift(
                 param.new(:name<length>,:c_attr(
                           'has uint16 $.length is rw;' ~ "\n" ~
                           'constant length___maxof = 0xffff;' ~ "\n"))
            );
        }
        else {
            $p<pad0_0>.c_attr = 'has uint8 $.pad0_0;'
                unless $p.params[0]:exists;
            $p.params.splice(1,0,
                 param.new(:name<length>,:c_attr(
                           'has uint16 $.length is rw;' ~ "\n" ~
                           'constant length___maxof = 0xffff;' ~ "\n"))
                 );
        }
        my $oc = $req.attribs<opcode>;
        if $mod.extension {
            $p.params.unshift(
                param.new(:name<minor_opcode>
                          :p2c_init("\$!minor_opcode = $oc;")
                          :c_attr('has uint8 $.minor_opcode is rw;'))
                );
            $oc = 0;
        }
        $p.params.unshift(
            param.new(:name<major_opcode>
                      :p2c_init("\$!major_opcode = $oc;")
                      :c_attr('has uint8 $.major_opcode is rw;'))
        );

        @cstructs.push(qq:to<EOCS>);

                our class cstruct does cpacking is repr("CStruct") \{

            {({ |(.c_doc, .c_attr) } for $p.params).join("\n").indent(8)}

                    method Hash \{
                        \{
            {$p.params».c2p_arg.join(",\n").indent(16)}
                        }
                    }
                    method nativize(\$p6) \{
            {$p.params».p2c_init.join("\n").indent(12)}
                    }

                };
                my \$.cstruct = cstruct;
            EOCS

        my $clname = $req.attribs<name>;
        $clname = $oname ~ $clname
            if $clname eq any <
                DestroyContext QueryVersion QueryExtension ListProperties
                CreateCursor GetVersion QueryBestSize SelectInput Bell
                Enable CreateContext ChangeSaveSet GetImage PutImage
                CreatePixmap
            > or $mod.cname eq "present"
              or ($clname ~~ /^Shm/) and $mod.cname eq "xv";
        my $isvoid = not $req.elements(:TAG<reply>);

        my @doc;
        @doc.append(MakeClassDocs($req, $clname ~ "Request"));

        $fdsend = qq:to<EOFD> if +($p.params».fd_send);
        # A higher level Connection object.
        multi method send(\$c) \{
            \$c.fd_lock.protect: \{
        {$p.params».fd_send.join("\n").indent(8)}
                \$.sequence = self.send(\$c.xcb);
            }
            my \$ret = Cookie.new(:\$.sequence, :reply_type(\$.reply));
            \$c.cookies.send(\$ret.vow);
            \$ret;
        }
        EOFD

        my $export = $(:DEFAULT, :requests);
        with %X11::XCBquirks::RequestExports{$mod.modname ~ "::" ~ $clname} {
            $export = $_;
        }
	$export = "is export" ~ @$export.perl;


        @p6classes.push(qq:to<EO6C>);
            {%GoodReqs{$isvoid ?? "Nil" !! "$oname $clname"} ?? "" !! "# TODOP6 Reply has TODOP6s"}
            {@doc.join("\n")}
            our class {$clname}Request
                does Request[OpcodeEnum::Opcode({$req.attribs<opcode>}),
                             {$mod.cname eq "xproto"
                                  ?? "xcb_extension_t"
                                  !! '&xcb_' ~ $mod.cname ~ "_id"},
                             {$isvoid.gist}]
                $flagroles
                $export \{

                my \$.reply{ $isvoid ?? ";" !! " = " ~ $clname ~ "Reply" ~ ";" }

                {$p.params».p_subclasses.join("\n").indent(4)}

                { @cstructs[*-1] }

                has \$.sequence is rw;
            {({ |(.p_doc, .p_attr) } for $p.params).join("\n").indent(4)}

            { makemeth_child_bufs($p, :indent(8)) }

            { makemeth_child_structs($p, :indent(8)) }

            {$fdsend.indent(4)}

            }
            EO6C
    }
    $mod.cstructs.append(@cstructs);
    $mod.p6classes.append(@p6classes);
}

sub MakeEpilogue($mod) {
    my $i = 0;
    my %selectors =
         "xproto" => qq:to<EOXP>,
              our class EventSelector does Selector[{$i++}] \{
                  method setrq (\$window, \$event-mask) \{
                      ChangeWindowAttributesRequest.new(:\$window,
                          :value_list(CWEnum::CWEventMask, \$event-mask)
                      )
                  }
                  method getrq (\$) \{ } # TODO
                  method mask (\$) \{ } # TODO
              }
              EOXP
         "xfixes" => qq:to<EOXF>,
              # In this case, pass an instance with the receiver :window
              our class EventSelector does Selector[{$i}] \{
                  has \$\.window;
                  method opcode \{ (\$\.window +< 8) +| {$i++} }
                  method setrq (\$selection, \$event_mask) \{
                      SelectSelectionInputRequest.new(
                          :\$\.window, :\$selection, :\$event_mask
                      )
                  }
                  method getrq (\$) \{ } # TODO
                  method mask (\$) \{ } # TODO
              }
              EOXF
         "screensaver" => qq:to<EOSS>,
              our class EventSelector does Selector[{$i++}] \{
                  method setrq (\$drawable, \$event_mask) \{
                      ScreenSaverSelectInputRequest.new(:\$drawable,:\$event_mask)
                  }
                  method getrq (\$) \{ } # TODO
                  method mask (\$) \{ } # TODO
              }
              EOSS
         ;

    $mod.epilogue = %selectors{$mod.cname} // "";
}

sub Output ($mod) {
    my $out = "./lib/X11/XCB/$mod.outname()".IO;
    $out .= open(:w);
    $out.print($mod.prologue ~ "\n");

    $mod.expexp{'EXPORT::' ~ $mod.modname ~ 'opcodes'} ~= # XXX should just be "EXPORT::opcodes"
        "our constant OpcodeEnum = ::X11::XCB::{$mod.modname}::OpcodeEnum;\n";
    for $mod.opcodes {
        my $const = "our constant {.value} = ::X11::XCB::{$mod.modname}::OpcodeEnum::Opcode::{.value};\n";
        $mod.expexp{'EXPORT::' ~ $mod.modname ~ 'opcodeenums'} ~= $const; # XXX should just be "EXPORT::opcodeenums"
        $mod.expexp{$mod.modname ~ '::OpcodeEnum::Opcode::EXPORT::opcodeenums'} ~= $const;
    }
    $out.print( qq:to<EOOC> ) if $mod.opcodes;
        our class OpcodeEnum # is export(:opcodes) XXX needs RT
        \{
            # is export(:opcodeenums) XXX needs RT
            our enum Opcode «
        { (":{.value}({.key})\n" for $mod.opcodes.sort(+*.key)).join.indent(8) }
            »;
        }

        EOOC
    $out.print($mod.enums.join);
    $out.print($mod.typedefs.join);
    $out.print($mod.p6classes.grep({$_ !~~ /TODOP6/}).join);

    for $mod.occludes.grep(!*.cstruct_done) -> $o {
        $out.print($o.cstruct);
    }

    $out.print($mod.epilogue);

    $out.print(qq:to<EOEH>);
        our \$errorcodes = :\{
        { (for $mod.errors.sort(+*.key) {
              $_.key ~ " => " ~ $_.value
           }).join(",\n").indent(4) }
        };
        our \$eventcodes = :\{
        { (for $mod.events.sort(+*.key) {
              $_.key ~ " => " ~ $_.value
           }).join(",\n").indent(4) }
        };
        our \$xgecodes = :\{
        { (for $mod.xge.sort(+*.key) {
              $_.key ~ " => " ~ $_.value
           }).join(",\n").indent(4) }
        };
        EOEH

    $out.print("\n}\n");

    for $mod.expexp -> ( :$key, :$value ) {
        $out.print: "package $key \{\n{$value.indent(4)}\n}\n";
    }

    say($mod.p6classes.grep({$_ ~~ /TODOP6/}).join);
    $out.close;
}

sub makeParasiticList($p, $f, *%) {
    # Here we expect a second list enabled by a bool
    # which has the same length of the list above it.
    my $name = $f.attribs<name>;
    my $type = $f.attribs<type>;
    # Should have two fieldrefs with a "*" op, one of
    # them should be boolean.
    (my $l_bool, my $l_int) =
        $f.elements(:TAG<fieldref>, :RECURSE).map: *.contents.Str;
    $p{$name}.p_attr = "has @.$name;";
    $p{$name}.c2p_code = qq:to<EOPC>;
        if \$pstruct.$l_bool \{
            \@args.append:
                "$name",
                (for 0..^\$pstruct\.$l_int \{
                     # XXX no assurance pointer math will not start adding by sizeof
                     $type\.new(Pointer.new(\$p + \$oleft - \$left),
                                :\$left, :!free);
                    }
                )
        }
        EOPC
    $p{$l_bool}.p_attr = |();
    $p{$l_bool}.c2p_arg = |();
    $p{$l_bool}.p2c_init = qq:to<EOPA>;
            \$\!$l_bool = +?\$p6.$name;
            die 'Wrong number of elements in $name'
                unless \$p6.$name\.elems == \$\!$l_int;
            EOPA
    $p{$name}.p2c_code = "\@bufs.push(\$_.bufs) for \@\.$name;"
}

sub makeClientMessageData($p, $f, *%_) {
    $p<data>.c_attr = q:to<EOCM>;
        has uint32 $.cmd___pad0;
        has uint32 $.cmd___pad1;
        has uint32 $.cmd___pad2;
        has uint32 $.cmd___pad3;
        has uint32 $.cmd___pad4;
        EOCM
    $p<format>.p_attr = '# Use depth of $.data for $.format';
    $p<format>.p2c_init = '$!format = nativesizeof($p6.data.of) +< 3;';
    $p<format>.c2p_arg = |();
    $p<data>.p_attr = 'has $.data;';
    $p<data>.c2p_arg = q:to<EOPC>;
        :data(
            do given self.format {
                when 8 {
                    Buf[uint8].new(
                        nativecast(CArray[uint8], self)[12..^32];
                    )
                }
                when 16 {
                    Buf[uint16].new(
                        nativecast(CArray[uint16], self)[6..^16];
                    )
                }
                when 32 {
                    Buf[uint32].new(
                        nativecast(CArray[uint32], self)[3..^8];
                    )
                }
            }
        )
        EOPC
    $p<data>.p2c_init =
        '($!cmd___pad0, $!cmd___pad1, $!cmd___pad2, ' ~
        '$!cmd___pad3, $!cmd___pad4)'
        ~ "\n" ~
        '    = nativecast(CArray[uint32],$p6.data)[^5] ';
}