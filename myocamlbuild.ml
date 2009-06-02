open Ocamlbuild_plugin
open Command

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
    let line = trim line in
    if line = "" || line.[0] = '#' then acc else
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
  let config = try parse_config "Config" with _ -> [] in
  fun value ->
    try List.assoc (String.uppercase value) config with Not_found -> ""

let config_yes x = String.uppercase (config x) = "YES"

let () = dispatch begin function
  | After_rules ->
      flag ["ocaml"; "doc"] (S[A "-hide-warnings"; Sh (config "OCAMLINCLUDES")]);

      ocaml_lib ~extern: true "mlpost";
      ocaml_lib ~extern: true "cairo";
      ocaml_lib ~extern: true "bitstring";

      if config "MLPOSTCAIRO" = "YES" then begin
        tag_any ["use_bigarray"; "use_bitstring"; "use_cairo"]
      end;

      let mlpost_onoff = config "MLPOSTSPECIFIC" in
      let mlpost_specific = "melt/mlpost_specific.ml" in
      rule
        mlpost_specific
        ~dep: mlpost_onoff
        ~prod: mlpost_specific
        (fun _ _ -> cp mlpost_onoff mlpost_specific)
  | _ -> ()
end
