unit package X11::XCBquirks;

# This package extracts (eventually) all the exemptions from systematic
# rules that were needed when coaxing X11 APIs into a sane
# and idiomatic namespace.  It is used by gen_from_xml.pm,
# but also available as a module for the purposes of
# documentation, or perhaps for programmatic uses.


#| Enum packages not exported by default due to inter-module
#| conflicts.
our %EnumExports =
    # Clash in Notify related enums between XProto and Input
    'XProto::NotifyMode' => (:notifyenums,),
    'XProto::NotifyDetail' => (:notifyenums,),
    'Input::NotifyMode' => (:notifyenums,),
    'Input::NotifyDetail' => (:notifyenums,),
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
# see %EnumExports
#    'Input::NotifyMode' => (:XI2enums,),
#    'Input::NotifyDetail' => (:XI2enums,),
    'Input::HierarchyMask' => (:XI2enums,),
    'Input::PropertyFlag' => (:XI2enums,),
    'Input::TouchEventFlags' => (:XI2enums,),
    'Input::TouchOwnershipFlags' => (:XI2enums,),
    'Present::Option' => (:optionenums,),
;


#| Inter-module imports or conflicts for structs
our %StructExports =
    # Imports needed from other modules... add to :internal tag
    'XProto::Rectangle' => (:DEFAULT, :structs, :internal),
    'XProto::String' => (:DEFAULT, :structs, :internal),
    'XProto::Point' => (:DEFAULT, :structs, :internal),
    'Render::Transform' => (:DEFAULT, :structs, :internal),
    'Xv::ImageFormatInfo' => (:DEFAULT, :structs, :internal),

;
