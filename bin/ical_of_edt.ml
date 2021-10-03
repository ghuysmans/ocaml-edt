open Icalendar

let generation_time =
  match Unix.gettimeofday () |> Ptime.of_float_s with
  | None -> failwith "Ptime.of_float_s"
  | Some t -> t

(* FIXME lang? *)
let mk_event ~id ~location start doe ?freq ?description summary = `Event {
  dtstamp = Params.empty, generation_time;
  uid = Params.empty, string_of_int id; (* FIXME *)
  dtstart = Params.empty, `Datetime (`Local start);
  dtend_or_duration = Some (
    match doe with
    | `Minutes m -> `Duration (Params.empty, Ptime.Span.of_int_s (m * 60))
    | `End e -> `Dtend (Params.empty, `Datetime (`Local e))
  );
  rrule = (
    match freq with
    | None -> None
    | Some (f, None) ->
      Some (Params.empty, (f, None, None, []))
    | Some (f, Some last) ->
      Some (Params.empty, (f, Some (`Until (`Local last)), None, []))
  );
  props = (
    `Location (Params.empty, location) ::
    `Summary (Params.empty, summary) ::
    match description with
    | None -> []
    | Some d -> [`Description (Params.empty, d)]
  );
  alarms = [];
}

module H = Hashtbl.Make (struct
  type t = [`Class | `Room | `Teacher] * string
  let hash = Hashtbl.hash
  let equal = (=)
end)

let import timetable =
  Csv.load ~separator:';' timetable |>
  List.tl |> (* skip the header *)
  List.map Edt.Timetable.of_list

let generate tz l =
  to_ics ([
    `Prodid (Params.empty, "ical_of_edt");
    `Version (Params.empty, "2.0");
    `Xprop (("WR", "TIMEZONE"), Params.empty, tz);
  ], l)

let add_span t span =
  match Ptime.add_span t span with
  | None -> failwith "Ptime.add_span"
  | Some t -> t

let interval_of_timetable default_duration fst (tt : Edt.Timetable.t) =
  let d =
    match tt.day with
    | Monday -> 0
    | Tuesday -> 1
    | Wednesday -> 2
    | Thursday -> 3
    | Friday -> 4
    | Saturday -> 5
    | Sunday -> 6
  in
  let open Re in
  let conv g =
    let i i = Group.get g i |> int_of_string in
    (  d * 24 * 60 * 60 +
     i 1 * 60 * 60 +
     i 2 * 60) |>
    Ptime.Span.of_int_s |>
    add_span fst
  in
  match
    let n = rep1 (rg '0' '9') in
    all (compile (seq [group n; set ".:h"; group n])) tt.hour
  with
  | [] -> assert false
  | [start] -> conv start, `Minutes default_duration
  | [start; stop] -> conv start, `End (conv stop)
  | _ -> failwith "invalid Hour range format"

let bulk (t : Spec.t) =
  let first =
    let t =
      match t.first with
      | Some f -> Spec.ptime_of_date f
      | None -> Spec.ptime_of_date (Ptime.to_date generation_time)
    in
    let dd =
      match Ptime.weekday t with
      | `Mon -> 0
      | `Tue -> 6
      | `Wed -> 5
      | `Thu -> 4
      | `Fri -> 3
      | `Sat -> 2
      | `Sun -> 1
    in
    add_span t (Ptime.Span.of_int_s (dd * 24 * 60 * 60))
  in
  let freq =
    match t.until with
    | None -> None
    | Some d -> Some (`Weekly, Some (Spec.ptime_of_date d))
  in
  let h = H.create 100 in
  import t.input |> List.iter (fun (tt : Edt.Timetable.t) ->
    let start, doe = interval_of_timetable t.slot_duration first tt in
    let add typ name description =
      match t.only with
      | Some o when o <> name -> ()
      | _ ->
        H.add h (typ, String.trim name) (mk_event
          ~id:tt.line
          ~location:(String.concat " / " tt.rooms)
          start
          doe
          ?freq
          description
        )
    in
    if t.generate_students then
      tt.classes |> List.iter (fun c ->
        add `Class c
          (if t.show_classes then
             tt.subject ^ " " ^ Edt.Comma.to_string tt.classes
           else
             match tt.teachers with
             | [] -> tt.subject
             | l -> tt.subject ^ ", " ^ Edt.Comma.to_string l)
      );
    if t.generate_rooms then
      tt.rooms |> List.iter (fun room ->
        add `Room room
          Edt.Comma.(to_string tt.teachers ^ " (" ^ to_string tt.classes ^ ")")
      );
    if t.generate_teachers then
      tt.teachers |> List.iter (fun teacher ->
        add `Teacher teacher
          (Edt.Comma.to_string tt.classes ^ " (" ^ tt.subject ^ ")")
      );
  );
  H.to_seq_keys h |>
  List.of_seq |> List.sort_uniq compare |>
  List.iter (fun ((_typ, name) as k) ->
    let fn =
      Re.(replace_string (compile (char '/')) ~by:"_") (name ^ ".ics") |>
      Filename.concat t.output_dir
    in
    let ch = open_out fn in
    Printf.printf "%s\n" fn;
    generate t.timezone (H.find_all h k) |> output_string ch;
    close_out ch
  )


let () =
  Cmdliner.Term.(exit @@ eval @@ (const bulk $ Spec.term, Spec.info))
