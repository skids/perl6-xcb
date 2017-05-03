unit package X11::XCBquirks;

# This package extracts (eventually) all the exemptions from systematic
# rules that were needed when coaxing X11 APIs into a sane
# and idiomatic namespace.  It is used by gen_from_xml.pm,
# but also available as a module for the purposes of
# documentation, or perhaps for programmatic uses.

# Note that currently the export tags are not carefully chosen
# and in fact are so badly named they will be changed.  For
# now the namespace detangling effort just needs these exports
# suppressed, naming them sanely will come later.

#| Enum values that are too common, and their common value.
#| Instead, a constant with this name and value is provided by
#| XProto.  If the value appears alone in an Enum, that Enum
#| is eliminated.  Otherwise, a stand-in with the name of the
#| whole enum prefixed takes its place.  Note that some
#| enum values with these names exists with different
#| integer values... in that case, the stand-in must be used
#| instead of the constant.
#|
#| Example: You can use either C<On>, or C<LedModeOn> for 0,
#| but you must use C<DPMSModeOn> if you want 1.
#|
#| notable exception: "Any" does not get a global constant
our %EnumValueConst =
    :None(0), :Success(0), :Insert(0), :Delete(1), :Normal(0),
    :Off(0), :On(1),
    :Any<Any>,              # no constant will be made
    :Round(1), :Solid(0);   # not "too common" but easy to do here

#| Enums we just skip because they are not needed after
#| occlusions and Selectors.

our %EnumSkips =
    'Present::Event' => True,      # Occluded
    'Present::EventMask' => True,  # replaced by Present::EventSelector.mask
;

#| Enums that cannot be patched up through exports and are badly
#| enough named to merit renaming them.
our %EnumRename =
    'XProto::GC' => 'GCparam',     # vs X11.pm6
;

#| Enum packages not exported by default due to inter-module
#| or internal namespace conflicts.
our %EnumExports =
    # Clash in Notify related enums between XProto and Input
    'XProto::NotifyMode' => (:xprotonotifyenum,),  # vs Input
    'XProto::NotifyDetail' => (:xprotonotifyenum,),# vs Input
    'XProto::ScreenSaver' => (),                   # vs ScreenSaver
    'Input::NotifyMode' => (:inputnotifyenum,),    # vs XProto
    'Input::NotifyDetail' => (:inputnotifyenum,),  # vs XProto
    'RandR::ModeFlag' => (:modeflagenum,),         # vs XF86Vidmode
    'RandR::Transform' => (:transformenum,),       # vs Render
    'RandR::Connection' => (:connectionenum,),     # vs X11.pm6
    'DRI2::EventType' => (:eventtypeenum,),        # vs xkb
    'Glx::GC' => (),                               # vs XProto
    'Test::Cursor' => (),                          # vs Perl6 core vs XProto
    'XProto::Cursor' => (),                        # vs Perl6 core vs Test
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
    'Input::XIEventMask' => (:evmaskenums,),       # vs XProto
    'Input::ValuatorMode' => (:valuatorenums),     # vs Sync vs xkb
    'Input::Device' => (:deviceenums),             # vs XProto vs xkb

    'XProto::NotifyMode' => (:notifyenums,),
    'XProto::NotifyDetail' => (:notifyenums,),
    'XProto::KB' => (:kbenums,),                   # internal conflict
    'XProto::Exposures' => (:exposureenums,),      # internal conflict
    'XProto::Blanking' => (:blankingenums,),       # internal conflict
    'XProto::ModMask' => (:modmaskenums,),         # internal conflict use subset
    'XProto::MapIndex' => (:mapindexenums,),       # internal conflict
    'XProto::GrabMode' => (:grabmodeenums,),       # vs Input

    'Present::Option' => (:optionenums,),
    'RandR::ModeFlag' => (:modeflagenums,),       # vs XF86videomode
    'RandR::SetConfig'  => (:configenums,),       # vs XProto vs Xv
    'RandR::Transform' => (:transformenums,),     # vs Render
    'xkb::DoodadType' => (:doodadenums,),         # internal conflict
    'xkb::LedClassResult' => (:ledenums,),        # internal maybe use a subset?
    'xkb::BellClassResult' => (:bellresenums,),   # internal maybe use a subset?
    'xkb::BellClass' => (:bellenums,),            # internal maybe use a subset?
    'xkb::ID' => (:idenums,),                     # internal maybe use a subset?
    'xkb::SwitchScreenFlag' => (:switchsenums,),  # vs Sync vs Input
    'xkb::Groups' => (:groupssenums,),            # vs XProto vs Input
    'xkb::BoolCtrlsHigh' => (:ctrlshienums,),     # internal conflict
    'xkb::BoolCtrlsLow' => (:ctrlsloenums,),      # internal conflict
    'xkb::Control' => (:controlenums,),           # internal conflict
    'xkb::SAIsoLockFlag' => (:saisoenums,),       # internal conflict
    'xkb::SA' => (:saenums,),                     # internal conflict
    'xkb::NKNDetail' => (:nknenums,),             # internal conflict
    'xkb::GBNDetail' => (:gbnenums,),             # internal conflict
    'Render::PictOp'  => (:pictopenums,),         # vs XProto
    'Render::SubPixel'  => (:subpixenums,),       # vs RandR
    'Xv::GrabPortStatus' => (:grabenums,),        # vs XProto vs RandR
    'Xv::VideoNotifyReason' => (:notifyenums,),   # vs XProto
    'Sync::ALARMSTATE' => (:alarmenums,),         # XProto
    'DRI2::EventType' => (:eventtypeenums,),      # vs xkb
;


#| Inter-module imports or conflicts for structs
our %StructExports =
    # Imports needed from other modules... add to :internal tag
    'XProto::Rectangle' => (:DEFAULT, :structs, :internal),
    'XProto::String' => (:DEFAULT, :structs, :internal),
    'XProto::Point' => (:DEFAULT, :structs, :internal),
    'Render::Transform' => (:DEFAULT, :structs, :internal),
    'Xv::ImageFormatInfo' => (:DEFAULT, :structs, :internal),

    'RandR::ModeInfo' => (:modeinfo,),            # vs XF86VideoMode
;

#| Inter-module conflicts for Requests/Replies
our %RequestExports =
    'XF86Dri::GetDeviceInfo' => (:direq,),
    'Xinerama::GetState' => (:gsreq,),
    # Things Glx thought it was a good idea to mimic
    'Glx::CreateWindow' => (),

    'DRI2::CopyRegion' => (:copyregion,),   # vs XFixes

    'Print::SelectInput' => (),             # common module interface
    'Shape::SelectInput' => (),             # common module interface
    'Present::SelectInput' => (),           # common module interface
    'XPrint::SelectInput' => (),            # common module interface
    'Xevie::SelectInput' => (),             # common module interface
    'RandR::SelectInput' => (),             # common module interface
    'ScreenSaver::SelectInput' => (),       # common module interface
;

#| Older xcbxml uses valueparam tags that need help finding
#| their correspondin enum
our %vpmap =
    'Print::SelectInputRequest' => 'EvMask',
    'Print::SelectInputReply'   => 'EvMask',
;

# The rest verges on mission creep for a quirks file, but
# each of these also serves to eliminate namespace conflicts.

#| Enums occluded behind sets of classes.  This avoids name
#| clashes between the enum values and the classes they are
#| used to choose.  To do this we simply have each class's
#| type object return the enum value's integer value when
#| numified, and add a few other bits of glue to emulate
#| most of the rest of the behaviors expected of an enum.
our @Occludes =
    "randr" => (
        :generic<NotifyData>, :class<NotifyData>, :envelope<NotifyEvent>,
        :typer<subCode>, :typee<u>, :enum<Notify>
    ),
    "xkb"   => (
        :generic<SIAction>, :class<Action>,
        :typer<type>, :typee<data>, :enum<SAType>,
        :toenum({ $_ ~~ /^SA(.*)$/; my $it = $/[0].Str; $it ~~ s/IsoLock/ISOLock/; $it }),
        :totype({ $_ ~~ s/ISOLock/IsoLock/; "SA$_" })
    ),
    "xkb"   => (
        :generic<CommonBehavior>, :class<Behavior>,
        :typer<type>, :typee<data>, :enum<BehaviorType>,
        :toenum({ $_ ~~ /^(.*?)Behavior$/; $/[0].Str}),
        :torole({ $_ ~~ /(.*?)\d*$/; $/[0] ~ "Behavior" }),
        :totype({ $_ ~ "Behavior" })
    )
;

my $i = 0;
our %Selectors =
     # xproto must be first here so it gets Selector[0]
     "xproto" => qq:to<EOXP>,
          our class EventSelector does Selector[{$i++}] \{
              method setrq (\$window, \$event-mask) \{
                  ChangeWindowAttributesRequest.new(:\$window,
                      :value_list(CWEnum::CWEventMask, \$event-mask)
                  )
              }
              method getrq (\$) \{ } # TODO
              method replymask (\$) \{ } # TODO
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
              method replymask (\$) \{ } # TODO
          }
          EOXF
     "screensaver" => qq:to<EOSS>,
          our class EventSelector does Selector[{$i++}] \{
              method setrq (\$drawable, \$event_mask) \{
                  SelectInputRequest.new(:\$drawable,:\$event_mask)
              }
              method getrq (\$) \{ } # TODO
              method replymask (\$) \{ } # TODO
          }
          EOSS
     "randr" => qq:to<EORR>,
          #| Instead of an enum, C<\%NotifyMask> maps the objects representing
          #| selectable events directly to bit values.  Note there are
          #| actually only two events, C<RandRScreenChangeEvent> and
          #| C<RandRNotifyEvent>.  The latter packs a number of variant
          #| structures.  Either the former event, or the structures
          #| that do the NotifyData role for the latter event may be
          #| used as keys here.  Or you can also pass a list of them to
          #| C<RandR::EventSelector.mask> and let it do the math.
          our \%NotifyMask is export(:DEFAULT, :selector) := :\{
              RandRScreenChangeNotifyEvent => 1,
              CrtcChange => 2,
              OutputChange => 4,
              OutputProperty => 8,
              ProviderChange => 16,
              ProviderProperty => 32,
              ResourceChange => 64
          };
          #| Mid-level utility class for selecting events from RandR.
          our class EventSelector does Selector[{$i}] \{
              #| For use by C<X11> module's C<Connection.follow> method,
              #| or can be used to keep track of windows from which
              #| events have been selected.  Not automatically used
              #| by the setrq method... use C<.request> for that.
              has \$\.window;
              #| (class or instance method)
              #| Get the mask value of a selectable event object
              method maskval (\$thing) \{ \%NotifyMask\{\$thing} }

              #| (class or instance method)
              #| Get the mask bit position of a selectable event object
              method maskbit (\$thing) \{
                  given \$thing \{
                      when NotifyData \{ +\$thing + 1 }
                      when RandRScreenChangeNotifyEvent \{ 0 }
                      default \{ Nil }
                  }
              }
              #| (class or instance method)
              #| Get an event mask selecting all provided event objects
              my method mask (*\@things) \{
                  [+|] \@things.map: \{ self.maskval(\$_) }
              }

              #| For use by C<X11> module's C<Connection.follow> method.
              method opcode \{ (\$\.window +< 8) +| {$i++} }

              #| (class or instance method)
              #| Build a request to have events matching C<\$event_mask>
              #| occurring on the offered C<\$drawable> delivered to
              #| a connection.
              method setrq (\$drawable, \$event_mask) \{
                  SelectInputRequest.new(:\$drawable,:\$event_mask)
              }
              #| Build a request to have events corresponding to the
              #| offered event objects when they occur on C<\$\.window>
              method request (EventSelector:D, *\@things) \{
                  self.setrq(\$\.window, self.mask(\@things))
              }
              method getrq (\$) \{ } # TODO
              method replymask (\$) \{ } # TODO
          }
          EORR
     "present" => qq:to<EORR>,
          #| Instead of an enum, C<\%EventMask> maps the objects representing
          #| selectable events directly to bit values. Or you can also pass a
          #| list of them to C<RandR::EventSelector.mask> and let it do the math.
          our \%EventMask is export(:DEFAULT, :selector) := :\{
              PresentConfigureNotifyXGEvent => 1,
              PresentCompleteNotifyXGEvent => 2,
              PresentIdleNotifyXGEvent => 4,
              PresentRedirectNotifyXGEvent => 8
          };
          #| Mid-level utility class for selecting events from Present.
          our class EventSelector does Selector[{$i}] \{
              #| For use by C<X11> module's C<Connection.follow> method,
              #| or can be used to keep track of windows from which
              #| events have been selected.  Not automatically used
              #| by the setrq method... use C<.request> for that.
              has \$\.window;

              #| Present is a newer module that leaves a way for the
              #| user to do what the C<X11> module's C<Connection.follow>
              #| does to track how many times an event has been selected,
              #| by allowing an extra unique id to be attached to each
              #| selection.  It will also echo them back in the received
              #| events.
	      has \$\.eid = 0;

              #| (class or instance method)
              #| Get the mask value of a selectable event object
              method maskval (\$thing) \{ \%EventMask\{\$thing} }

              #| (class or instance method)
              #| Get the mask bit position of a selectable event object
              method maskbit (\$thing) \{
                  given \$thing \{
                      when XGEvent \{ \$thing.event_code + 1 }
                      default \{ Nil }
                  }
              }
              #| (class or instance method)
              #| Get an event mask selecting all provided event objects
              my method mask (*\@things) \{
                  [+|] \@things.map: \{ self.maskval(\$_) }
              }

              #| For use by C<X11> module's C<Connection.follow> method.
              method opcode \{ (\$\.window +< 40) +| (\$\.eid +< 8) +| {$i++} }

              #| (class or instance method)
              #| Build a request to have events matching C<\$event_mask>
              #| occurring on the offered C<\$drawable> delivered to
              #| a connection.
              method setrq (\$drawable, \$event_mask, :\$eid = 0) \{
                  SelectInputRequest.new(
                      :\$drawable,:\$event_mask,:\$eid
                  )
              }

              #| Build a request to have events corresponding to the
              #| offered event objects when they occur on C<\$\.window>
              method request (EventSelector:D, *\@things, :\$eid) \{
                  self.setrq(\$\.window, self.mask(\@things), :\$eid)
              }
              method getrq (\$) \{ } # TODO
              method replymask (\$) \{ } # TODO
          }
          EORR
;

