type t = string list

let of_string = function
  | "" -> []
  | s -> String.split_on_char ',' s

let to_string = String.concat ","
