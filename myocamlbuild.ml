open Ocamlbuild_plugin

let parse_config file =
  let trim s =
    let is_blank = function
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false
    in
    let len = String.length s in
    let rec left n = if n < len && is_blank s.[n] then left (n+1) else n in
    let rec right n = if n >= 0 && is_blank s.[n] then right (n-1) else n in
    let left = left 0 and right = right (len-1) in
    if left < right then String.sub s left (right-left+1) else ""
  in
  let parse_config_line acc line =
    try
      let i = String.index line '=' in
      let left = String.sub line 0 i in
      let right = String.sub line (i + 1) (String.length line - i - 1) in
      (String.uppercase (trim left), trim right) :: acc
    with Not_found -> acc
  in
  let f = open_in file in
  let lines = ref [] in
  try
    while true do
      lines := input_line f :: !lines
    done;
    assert false
  with End_of_file ->
    List.fold_left parse_config_line [] !lines

let config =
  let config = parse_config "Config" in
  fun value -> List.assoc (String.uppercase value) config

let config_yes x = String.uppercase (config x) = "YES"

let () = dispatch begin function
  | After_rules ->
      let mlpost = config_yes "MLPOST" in
      let mlpost_onoff =
        if mlpost then "melt/mlpost_on.ml" else "melt/mlpost_off.ml" in
      let mlpost_specific = "melt/mlpost_specific.ml" in
      rule
        mlpost_specific
        ~dep: mlpost_onoff
        ~prod: mlpost_specific
        (fun _ _ -> cp mlpost_onoff mlpost_specific)
  | _ -> ()
end