open Printf

let includes = ref []

type verbatim_item = [
| `V of string
| `C of Format.formatter -> unit -> unit
| `M of Format.formatter -> unit -> unit
| `T of Format.formatter -> unit -> unit
]

type verbatim_function = Format.formatter -> verbatim_item list -> unit

let verbatim_functions: (string, verbatim_function) Hashtbl.t = Hashtbl.create 7

let find name =
  let rec f = function
    | [] ->
        eprintf "Error: Cannot find plugin %s.\n" name;
        exit 2
    | i::r ->
        let name = Filename.concat i name in
        if Sys.file_exists (name^".cmo") then name^".cmo" else
          if Sys.file_exists (name^".cma") then name^".cma" else
            f r
  in f !includes

let load_plugin =
  let init = ref false in
  fun name ->
    if not !init then begin
      Dynlink.init ();
      Dynlink.prohibit ["Ast"; "Lexer"; "Parser"; "Plugin_private"];
      init := true;
    end;
    try
      Dynlink.loadfile (find name)
    with Sys_error s ->
      eprintf "%s\nError: Cannot load plugin %s.\n" s name;
      exit 2
