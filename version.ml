let major = 1
let minor = 0
let release = 1
let full =
  string_of_int major ^ "." ^ string_of_int minor ^ "." ^ string_of_int release

let print () = Printf.printf "%s\n%!" full
