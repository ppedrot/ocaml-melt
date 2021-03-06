(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the  name of Melt nor  the names of its  contributors may be *)
(*   used  to endorse  or  promote products  derived  from this  software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

open Printf

(* From Totoconf *)
exception Exec_error of int

let try_finally f g =
  let result =
    try
      f ();
    with exn ->
      g ();
    raise exn
  in
  (g (): unit);
  result

let exec_line cmd args =
  let c = String.concat " " (cmd::args) in
  let tmp = Filename.temp_file "configure_exec_line" ".out" in
  try_finally
    (fun () ->
      try
        match Sys.command (c ^ " > " ^ tmp) with
          | 0 -> input_line (open_in tmp)
          | n -> raise (Exec_error n)
      with End_of_file ->
        "")
    (fun () -> Sys.remove tmp)
(* *)

let files = Queue.create ()
let main_file = ref ""

let mlpost = ref Melt.compiled_with_mlpost
let ocamlbuild = ref true
let native = ref false
let final = ref true
let link = ref true

let dvi = ref false
let pdf = ref false
let ps2pdf = ref false
let cairo = ref false
let mps = ref false
let quiet = ref false
let continue = ref false
let fake = ref false

let bibtex = ref false
let fast = ref false

let clean = ref false

let melt_dir = ref "_melt"

let latex = ref None

let meltpp = ref "meltpp"
let latop = ref "latop"
let mlpost_bin = ref "mlpost"
let mlpost_no_prelude = ref false

let classic_display = ref false

(* -P includes for meltpp *)
let plugin_includes = ref []
let meltpp_plugin_includes = ref ""
let add_plugin_include x = plugin_includes := x :: !plugin_includes

(* -I includes for the OCaml compiler *)
let includes = ref []
let add_include x = includes := x :: !includes

(* -L add links for latex *)
let latex_link = ref []
let add_latex_link x = latex_link := x :: !latex_link

let resto = ref ""
let set_rest s =
  match !resto with
    | "" -> resto := s
    | r -> resto := r ^ " " ^ s

let () =
  try
    let libmelt_dir = exec_line "ocamlfind" ["query"; "melt"; "2> /dev/null"] in
    add_include libmelt_dir
  with Exec_error _ -> add_include "+melt"

let spec = Arg.align [
  "-meltpp", Arg.Set_string meltpp, "<meltpp> Specify the location of the \
Melt pre-processor";
  "-latop", Arg.Set_string latop, "<latop> Specify the location of latop";
  "-mlpost", Arg.Set_string mlpost_bin,
  "<mlpost> Specify the location of the mlpost tool";
  "-mlpost-no-prelude", Arg.Set mlpost_no_prelude,
  " Do not pass a -latex option to mlpost";
  "-latex", Arg.String(fun cmd -> latex := Some cmd),
  "<latex> Specify the latex command to use";
  "-P", Arg.String add_plugin_include, "<dir> Look for plugins in <dir> \
(this option is passed to the Melt pre-processor)";
  "-I", Arg.String add_include, "<dir> Look for libraries in <dir> \
(this option is passed to the OCaml compiler)";
  "-L", Arg.String add_latex_link,
  "<path> Add a link to the path in the _melt directory";

  "-classic-display", Arg.Set classic_display,
  " Call Ocamlbuild with -classic-display (do not work with Mlpost)";
  "-no-mlpost", Arg.Clear mlpost, " Do not call mlpost, use ocamlbuild instead \
(or ocamlc if -no-ocamlbuild)";
  "-no-ocamlbuild", Arg.Clear ocamlbuild, " Do not use Ocamlbuild";
  "-no-final", Arg.Clear final, " Do not produce the PS or the PDF";
  "-no-link", Arg.Clear link, " Do not create a symbolic link to the PS or PDF";

  "-byte", Arg.Clear native, " Compile to byte code (default)";
  "-native", Arg.Set native, " Compile to native code instead of bytecode";

  "-dvi", Arg.Set dvi, " Produce a DVI instead of a PS";
  "-ps", Arg.Clear pdf, " Produce a PS file (this is the default behavior)";
  "-pdf", Arg.Set pdf, " Produce a PDF instead of a PS";
  "-ps2pdf", Arg.Set ps2pdf,
  " Produce a PS, then convert it to PDF using ps2pdf";
  "-cairo", Arg.Unit (fun () -> cairo := true; pdf := true),
  " Use the Cairo backend of Mlpost (implies -pdf)";
  "-mps", Arg.Unit (fun () -> cairo := true; pdf := true),
  " Use the native Mps backend of Mlpost (implies -pdf)";
  "-quiet", Arg.Set quiet, " Be quiet";
  "-q", Arg.Set quiet, " Same as -quiet";
  "-continue", Arg.Set continue, " Continue on errors";
  "-k", Arg.Set continue, " Same as -continue";
  "-fake", Arg.Set fake, " Do not actually execute commands";
  "-n", Arg.Set fake, " Same as -fake";

  "-bibtex", Arg.Set bibtex, " Use BibTeX";
  "-fast", Arg.Set fast, " Do not call LaTeX again to get references right";

  "-melt-dir", Arg.Set_string melt_dir, "<dir> Change the name used for \
the _melt directory";

  "-clean", Arg.Set clean, " Remove the _melt directory and, if not -no-link, \
all symbolic links of the current directory linking into _melt \
(cleaning is done before anything else)";

  "-version", Arg.Unit Melt_version.print, " Print version";
  "--", Arg.Rest set_rest,
  " Pass the remaining arguments to the generated program";
]
let anon s =
  main_file := s;
  Queue.add s files
let usage =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [options] [other_files] main_file\n
All [other_files] will be copied in the _melt directory. In particular, this \
allows you to use other modules, libraries or Ocamlbuild plugins. Files \
with extension .mlt will be pre-processed, compiled and executed.\n"

let cmd x = ksprintf begin fun s ->
  if not !quiet then printf "%s\n%!" s;
  if not !fake then
    let code = Sys.command s in
    if code <> 0 && not !continue then exit code
end x

let mlpost_version = ref ""

let check_mlpost_version () =
  try
    mlpost_version := Totoconf.exec_line !mlpost_bin ["-version"]
  with
    | Totoconf.Exec_error _ ->
        ()

let mlpost_version_ge s =
  !mlpost_version = "current" || Totoconf.Version.ge !mlpost_version s

let mlpost_version_le s =
  !mlpost_version <> "current" && Totoconf.Version.le !mlpost_version s

let melt_to_ml f =
  let o = Filename.chop_extension f ^ ".ml" in
  cmd "%s%s -dir \"../\" -open Latex -open Melt %s -o %s" !meltpp
    !meltpp_plugin_includes f o

let libopt lib =
  let dot_cma = if !native then ".cmxa" else ".cma" in
  if !mlpost then
    " -ccopt " ^ if !ocamlbuild then "\"-lib " ^ lib ^ "\"" else lib ^ dot_cma
  else if !ocamlbuild then " -lib \"" ^ lib ^ "\"" else " "^lib^dot_cma

let ml_to_tex f =
  let bf = Filename.chop_extension f in
  let pdfo =
    if !pdf then " -pdf" else
      if mlpost_version_ge "0.7" then
        " -ps"
      else
        " -pdf"
  in
  let pdfeo = if !pdf then " -execopt \"-pdf\"" else "" in
  let nameo = " -name " ^ bf in
  let nameeo = " -execopt \"-name " ^ bf ^ "\"" in
  let ocamlbuildo = if !ocamlbuild then " -ocamlbuild" else "" in
  let nativeo = if !native then " -native" else "" in
  let latexlibo = libopt "latex" in
  let meltlibo = libopt "melt" in
  let mlpostlibo =
    if Melt.compiled_with_mlpost then libopt "mlpost" else "" in
  let strlibo = libopt "str" in
  let unixlibo = libopt "unix" in
  let ext = if !native then "native" else "byte" in
  let ocamlc_includes = match !includes with
    | [] -> ""
    | l -> " -I " ^ String.concat " -I " l
  in
  let ocamlbuild_includes = match !includes with
    | [] -> ""
    | l ->
        let includes = "-I," ^ String.concat ",-I," l in
        " -cflags " ^ includes ^ " -lflags " ^ includes
  in
  let mlpost_includes = match !includes with
    | [] -> ""
    | l ->
        " -ccopt \"" ^
          (if !ocamlbuild then ocamlbuild_includes else ocamlc_includes)
        ^ "\""
  in
  let classicdisplayo = if !classic_display then " -classic-display" else "" in
  let mlpost_preludeo =
    if !mlpost_no_prelude then "" else
      let prelude_file = bf ^ ".tex" in
      if Sys.file_exists prelude_file then
        " -latex " ^ prelude_file
      else ""
  in
  if !mlpost then
    cmd "%s -v%s%s%s%s%s%s%s%s%s%s%s%s %s"
      !mlpost_bin
      mlpost_preludeo
      (if mlpost_version_ge "0.7" && mlpost_version_le "0.7.1" then
         classicdisplayo
       else "")
      mlpost_includes
      pdfo
      (" -execopt \"" ^ (String.escaped !resto) ^ "\"")
      (if !cairo then " -cairo -execopt \"-cairo\"" 
       else if !mps then " -mps -execopt \"-mps\""
       else pdfeo)
      ocamlbuildo nativeo
      strlibo latexlibo meltlibo nameeo f
  else if !ocamlbuild then
    cmd "ocamlbuild%s%s%s%s%s%s%s %s.%s --%s%s%s"
      classicdisplayo
      ocamlbuild_includes
      strlibo unixlibo latexlibo mlpostlibo meltlibo bf ext pdfo nameo !resto
  else begin
    cmd "ocaml%s%s%s%s%s%s%s %s -o %s.%s"
      (if !native then "opt" else "c")
      ocamlc_includes
      strlibo unixlibo latexlibo mlpostlibo meltlibo f bf ext;
    cmd "./%s.%s%s%s%s" bf ext pdfo nameo !resto
  end

let handle_auxiliary_file f =
  if Filename.check_suffix f ".mlt" then
    melt_to_ml f

let produce_final f =
  let bf = Filename.chop_extension f in

  let tex = bf ^ ".tex" in
  if not (Sys.file_exists tex) then begin
    Printf.eprintf "Error: cannot find file: \"%s\" (in the \"%s\" directory).\nMaybe the \"emit\" function has not been called on your document?\n%!" tex !melt_dir;
    exit 2
  end;

  let add_link s =
    cmd "ln -fs ../%s %s" s s
  in
  List.iter add_link !latex_link;

  let latex =
    match !latex with
      | None ->
          if !ps2pdf
          then "latex"
          else
            if !pdf
            then "pdflatex"
            else "latex"
      | Some cmd ->
          cmd
  in
  let latex =
    latex ^ " -interaction nonstopmode -file-line-error -halt-on-error"
  in
  let latop = sprintf " | %s > /dev/null" !latop in

  cmd "%s %s%s" latex bf latop;
  if !bibtex then begin
    cmd "bibtex %s" bf;
    cmd "%s %s%s" latex bf latop
  end;
  if not !fast then
    cmd "%s %s%s" latex bf latop;

  if not !pdf && not !dvi then
    cmd "dvips %s" bf;

  if !ps2pdf then begin
    if !pdf || !dvi then
      cmd "dvips %s" bf;
    cmd "ps2pdf %s.ps" bf
  end

let handle_main_file f =
  if Filename.check_suffix f ".mlt" || Filename.check_suffix f ".ml" then begin
    let ml = Filename.chop_extension f ^ ".ml" in
    ml_to_tex ml;
  end;
  if !final then produce_final f

let produce_link f =
  let bf = Filename.chop_extension f in
  let o =
    if !pdf || !ps2pdf then bf ^ ".pdf" else
      if !dvi then bf ^ ".dvi" else
        bf ^ ".ps" in
  cmd "ln -f -s %s/%s %s" !melt_dir o o

let chdir d =
  if not !quiet then
    if !fake then
      Printf.printf "cd %s\n%!" d
    else
      Printf.printf "melt: Entering directory `%s'\n%!" d;
  if not !fake then Sys.chdir d

let make_temp_dir () =
  let dir = !melt_dir in
  cmd "mkdir -p %s" dir;
  Queue.iter begin fun f ->
    cmd "cp -f %s %s/%s" f dir f;
  end files;
  chdir dir

let prefix s t =
  if String.length s > String.length t then false else
    String.compare s (String.sub t 0 (String.length s)) = 0

let do_clean () =
  let dir = !melt_dir in
  cmd "rm -rf %s" dir;
  if !link then begin
    let cwd = Unix.opendir (Sys.getcwd ()) in
    begin try while true do
      let f = Unix.readdir cwd in
      if (Unix.lstat f).Unix.st_kind = Unix.S_LNK then
        if prefix dir (Unix.readlink f) then
          cmd "rm -f %s" f
    done with End_of_file -> () end;
    Unix.closedir cwd
  end

let () =
  Arg.parse spec anon usage;
  check_mlpost_version ();
  meltpp_plugin_includes := begin match !plugin_includes with
    | [] -> ""
    | l -> " " ^ String.concat " " (List.map (fun x -> "-P "^x) l)
  end;
  if !clean then do_clean ();
  if !main_file <> "" then begin
    let cwd = Sys.getcwd () in
    make_temp_dir ();
    Queue.iter handle_auxiliary_file files;
    handle_main_file !main_file;
    chdir cwd;
    if !final && !link then
      produce_link !main_file
  end
