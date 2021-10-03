type t = {
  line: int;
  classes: Comma.t;
  teachers: Comma.t;
  subject: string;
  rooms: Comma.t;
  day: Day.t;
  hour: string;
  duration: int;
}

let header = [
  "NUMERO";
  "CLASSE";
  "PROF_NOM";
  "MAT_LIBELLE";
  "SALLE";
  "JOUR";
  "H.DEBUT";
  "DUREE";
]

let of_list = function
  | [line; classes; teachers; subject; rooms; day; hour; duration] -> {
    line = int_of_string line;
    classes = Comma.of_string classes;
    teachers = Comma.of_string teachers;
    subject;
    rooms = Comma.of_string rooms;
    day = Day.of_string day;
    hour;
    duration = Scanf.sscanf duration "%dh00" (fun x -> x)
  }
  | _ -> failwith "Timetable.of_list"

let to_list {line; classes; teachers; subject; rooms; day; hour; duration} = [
  string_of_int line;
  Comma.to_string classes;
  Comma.to_string teachers;
  subject;
  Comma.to_string rooms;
  Day.to_string day;
  hour;
  Printf.sprintf "%dh00" duration;
]
