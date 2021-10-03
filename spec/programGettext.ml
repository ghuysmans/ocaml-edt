module Gettext = Gettext.Program (struct
  let textdomain = "ical_of_edt"
  let codeset = None
  let dir = Some "po" (* FIXME *)
  let dependencies = Gettext.init
end) (GettextCamomile.Map)
