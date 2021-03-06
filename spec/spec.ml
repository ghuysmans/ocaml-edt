module ProgramGettext = ProgramGettext
open ProgramGettext.Gettext

type date = Ptime.date

type t = {
  timezone: string;
  hour: int;
  first: date option;
  until: date option;
  only: string option;
  generate_teachers: bool;
  generate_students: bool;
  show_classes: bool;
  generate_rooms: bool;
  input: string;
  output_dir: string;
}


open Cmdliner

let hour =
  let doc = s_ "number of minutes in an hour" in
  Arg.(value & opt int 60 & info ~doc ["H"; "hour"])

let ptime_of_date d =
  match Ptime.of_date d with
  | Some t -> t
  | None -> failwith "Ptime.of_date"

let date =
  let open Ptime in
  Arg.conv ~docv:(s_ "date") ((fun x ->
    match of_rfc3339 (x ^ "T00:00:00Z") |> rfc3339_error_to_msg with
    | Ok (t, _, _) -> Ok (Ptime.to_date t)
    | Error e -> Error e
  ), fun fmt d ->
    String.sub (Ptime.to_rfc3339 (ptime_of_date d)) 0 10 |>
    Format.pp_print_string fmt
  )

let first =
  let doc = s_ "start from a given date" in
  Arg.(value & opt (some date) None & info ~doc ["from"])

let until =
  let doc = s_ "repeat events weekly before a given date" in
  Arg.(value & opt (some date) None & info ~doc ["u"; "until"; "repeat-until"])

let generate_teachers =
  let doc = s_ "generate teacher schedules" in
  Arg.(value & flag & info ~doc ["t"; "teachers"])

let generate_students =
  let doc = s_ "generate student schedules" in
  Arg.(value & flag & info ~doc ["s"; "students"])

let generate_rooms =
  let doc = s_ "generate room schedules" in
  Arg.(value & flag & info ~doc ["r"; "rooms"])

let output_dir =
  let doc = s_ "output directory" in
  Arg.(value & opt dir "." & info ~doc ["output-dir"])

let only =
  let doc = s_ "generate a single schedule" in
  Arg.(value & opt (some string) None & info ~doc ["only"])

let show_classes =
  let doc = s_ "show classes in student schedules" in
  Arg.(value & flag & info ~doc ["c"; "show-classes"])

let timezone =
  let doc = s_ "timezone" in
  Arg.(value & opt string "Europe/Brussels" & info ~doc ["T"; "timezone"])

let input =
  Arg.(required & pos 0 (some file) None & info ~docv:"timetable.csv" [])


let prefix ~timetable =
  let s = Filename.basename timetable in
  let suffix = "timetable.csv" in
  let open String in
  if length s >= length suffix then
    let p = length s - length suffix in
    if sub s p (length suffix) = suffix then
      Some (sub s 0 p)
    else
      None
  else
    None

let () =
  assert (prefix ~timetable:"/a/b/whatever.csv" = None);
  assert (prefix ~timetable:"/a/b/timetable.csv" = Some "");
  assert (prefix ~timetable:"/a/b/x_timetable.csv" = Some "x_")


let argv prog t =
  let open Dsl in
  let date = conv date in
  [[prog]] |>
  named "T" string t.timezone |>
  option "only" string t.only |>
  named "H" int t.hour |>
  option "from" date t.first |>
  option "u" date t.until |>
  flag "t" t.generate_teachers |>
  flag "c" t.show_classes |>
  flag "s" t.generate_students |>
  flag "r" t.generate_rooms |>
  named "output-dir" string t.output_dir |>
  list string [t.input] |>
  List.rev |>
  List.flatten |>
  Array.of_list


open Term

let term =
  (* FIXME this is awful *)
  let f timezone only hour
        first until
        generate_teachers
        show_classes generate_students
        generate_rooms
        input output_dir =
    {timezone; only; hour;
     first; until;
     generate_teachers;
     show_classes; generate_students;
     generate_rooms;
     input; output_dir}
  in
  const f $
    timezone $ only $ hour $
    first $ until $
    generate_teachers $
    show_classes $ generate_students $
    generate_rooms $
    input $ output_dir

let info =
  let doc = s_ "generate iCal schedules using CSV files exported from EDT" in
  info "ical_of_edt" ~doc
