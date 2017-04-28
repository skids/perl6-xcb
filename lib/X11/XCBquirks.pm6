unit package X11::XCBquirks;

# This package extracts (eventually) all the exemptions from systematic
# rules that were needed when coaxing X11 APIs into a sane
# and idiomatic namespace.  It is used by gen_from_xml.pm,
# but also available as a module for the purposes of
# documentation, or perhaps for programmatic uses.


# Enum name/value mappings whose names should be prefixed with
# their module name, or if they stand alone in an enum with
# no other values, eliminated entirely.
our %EnumValueConst = :None(0), :Success(0), :Normal(0);

#| Enum packages not exported by default due to inter-module
#| or internal namespace conflicts.
our %EnumExports =
    # Clash in Notify related enums between XProto and Input
    'XProto::NotifyMode' => (:xprotonotifyenum,),  # vs Input
    'XProto::NotifyDetail' => (:xprotonotifyenum,),# vs Input
    'Input::NotifyMode' => (:inputnotifyenum,),    # vs XProto
    'Input::NotifyDetail' => (:inputnotifyenum,),  # vs XProto
    'RandR::ModeFlag' => (:modeflagenum,),         # vs XF86Vidmode
    'RandR::Transform' => (:transformenum,),       # vs Render
;

#| Enums which do not get exported under the ":enums" tag
#| for one reason or another, and what tags they get instead.
#| Note the values .perl out to how they need to appear in
#| generated code.
#|
#| Sometimes we include non-conflicting enums in here because
#| it makes more sense to group similar enums together.
our %EnumValueExports =
    # XI2 introductions
    'Input::Device' => (:XI2enums,),
    'Input::HierarchyChangeType' => (:XI2enums,),
    'Input::ChangeMode' => (:XI2enums,),
    'Input::DeviceClassType' => (:XI2enums,),
    'Input::DeviceType' => (:XI2enums,),
    'Input::ScrollFlags' => (:XI2enums,),
    'Input::ScrollType' => (:XI2enums,),
    'Input::TouchMode' => (:XI2enums,),
    'Input::GrabOwner' => (:XI2enums,),
    'Input::EventMode' => (:XI2enums,),
    'Input::GrabMode22' => (:XI2grabenums,),
    'Input::GrabType' => (:XI2grabenums,),
    'Input::ModifierMask' => (:XI2enums,),
    'Input::ChangeReason' => (:XI2enums,),
    'Input::KeyEventFlags' => (:XI2enums,),
    'Input::PointerEventFlags' => (:XI2enums,),
    'Input::NotifyMode' => (:XI2enums,),
    'Input::NotifyDetail' => (:XI2enums,),
    'Input::HierarchyMask' => (:XI2enums,),
    'Input::PropertyFlag' => (:XI2enums,),
    'Input::TouchEventFlags' => (:XI2enums,),
    'Input::TouchOwnershipFlags' => (:XI2enums,),
    'Input::FeedbackClass' => (:feedbackenums,),   # vs XProto
    'Input::XIEventMask' => (:evmaskenums,),       # vs XProto. Maybe just a utility combiner if values all consistent with event numbers

    'XProto::NotifyMode' => (:notifyenums,),
    'XProto::NotifyDetail' => (:notifyenums,),
    'XProto::KB' => (:kbenums,),                  # internal conflict
    'XProto::Exposures' => (:exposureenums,),     # internal conflict
    'XProto::Blanking' => (:blankingenums,),     # internal conflict


    'Present::Option' => (:optionenums,),
    'RandR::ModeFlag' => (:modeflagenums,),       # vs XF86videomode
    'RandR::SetConfig'  => (:configenums,),       # vs XProto vs Xv
    'RandR::Transform' => (:transformenums,),     # vs Render
    'xkb::DoodadType' => (:doodadenums,),         # internal conflict
    'xkb::LedClassResult' => (:ledenums,),        # internal maybe use a subset?
    'xkb::BellClassResult' => (:bellresenums,),   # internal maybe use a subset?
    'xkb::BellClass' => (:bellenums,),            # internal maybe use a subset?
    'xkb::ID' => (:idenums,),                     # internal maybe use a subset?
    'Render::PictOp'  => (:pictopenums,),         # vs XProto
    'Render::SubPixel'  => (:subpixenums,),       # vs RandR
    'Xv::GrabPortStatus' => (:grabenums,),        # vs XProto vs RandR
    'Sync::ALARMSTATE' => (:alarmenums,),         # XProto

;


#| Inter-module imports or conflicts for structs
our %StructExports =
    # Imports needed from other modules... add to :internal tag
    'XProto::Rectangle' => (:DEFAULT, :structs, :internal),
    'XProto::String' => (:DEFAULT, :structs, :internal),
    'XProto::Point' => (:DEFAULT, :structs, :internal),
    'Render::Transform' => (:DEFAULT, :structs, :internal),
    'Xv::ImageFormatInfo' => (:DEFAULT, :structs, :internal),
    'RandR::ModeInfo' => (:modeinfo,),
;

#| Inter-module conflicts for Requests/Replies
our %RequestExports =
    'XF86Dri::GetDeviceInfo' => (:direq,),
    'Xinerama::GetState' => (:gsreq,),
    # Things Glx thought it was a good idea to mimic
    'Glx::CreateWindow' => (:xproto,),
;
