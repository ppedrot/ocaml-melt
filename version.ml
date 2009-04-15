let major = 1
let minor = 2
let release = 0
let full =
  string_of_int major ^ "." ^ string_of_int minor ^ "." ^ string_of_int release

let print () = Printf.printf "%s\n%!" full
