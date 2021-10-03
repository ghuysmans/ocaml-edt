type t = {
  line: int;
  classes: Comma.t;
  teachers: Comma.t;
  subject: string;
  rooms: Comma.t;
  day: Day.t;
  start: int * int;
  duration: int * int;
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

let time_of_string s = Scanf.sscanf s "%dh%d" (fun x y -> x, y)
let string_of_time (h, m) = Printf.sprintf "%dh%02d" h m

let of_list = function
  | [line; classes; teachers; subject; rooms; day; start; duration] -> {
    line = int_of_string line;
    classes = Comma.of_string classes;
    teachers = Comma.of_string teachers;
    subject;
    rooms = Comma.of_string rooms;
    day = Day.of_string day;
    start = time_of_string start;
    duration = time_of_string duration;
  }
  | _ -> failwith "Timetable.of_list"

let to_list {line; classes; teachers; subject; rooms; day; start; duration} = [
  string_of_int line;
  Comma.to_string classes;
  Comma.to_string teachers;
  subject;
  Comma.to_string rooms;
  Day.to_string day;
  string_of_time start;
  string_of_time duration;
]
