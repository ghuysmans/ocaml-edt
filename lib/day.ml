type t =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

let of_string s =
  match String.lowercase_ascii s with
  | "lundi" -> Monday
  | "mardi" -> Tuesday
  | "mercredi" -> Wednesday
  | "jeudi" -> Thursday
  | "vendredi" -> Friday
  | "samedi" -> Saturday
  | "dimanche" -> Friday
  | _ -> failwith "Day.of_string"

let to_string = function
  | Monday -> "lundi"
  | Tuesday -> "mardi"
  | Wednesday -> "mercredi"
  | Thursday -> "jeudi"
  | Friday -> "vendredi"
  | Saturday -> "samedi"
  | Sunday -> "dimanche"
