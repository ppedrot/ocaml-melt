type verbatim_item = [ `V of string | `A of Format.formatter -> unit -> unit ]

type verbatim_function = Format.formatter -> verbatim_item list -> unit

let verbatim_functions: (string, verbatim_function) Hashtbl.t = Hashtbl.create 7

let load_plugin =
  let init = ref false in
  fun name ->
    if not !init then begin
      Dynlink.init ();
      Dynlink.prohibit ["Ast"; "Lexer"; "Parser"; "Plugin_private"];
      init := true;
    end;
    try
      let name = name ^ ".cma" in
      Dynlink.loadfile name
    with
      | Sys_error s ->
          Printf.eprintf "%s\nError: Cannot load plugin %s.\n" s name;
          exit 2
