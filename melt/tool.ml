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

let files = Queue.create ()
let main_file = ref ""

let mlpost = ref Melt.compiled_with_mlpost
let ocamlbuild = ref true
let native = ref true
let final = ref true
let link = ref true

let dvi = ref false
let pdf = ref false
let quiet = ref false
let continue = ref false
let fake = ref false

let bibtex = ref false
let fast = ref false

let clean = ref false

let melt_dir = ref "_melt"

let meltpp = ref "meltpp"

let classic_display = ref false

(* -P includes for meltpp *)
let plugin_includes = ref []
let meltpp_plugin_includes = ref ""
let add_plugin_include x = plugin_includes := x :: !plugin_includes

(* -I includes for the OCaml compiler *)
let includes = ref []
let add_include x = includes := x :: !includes

let spec = Arg.align [
  "-meltpp", Arg.Set_string meltpp, "<meltpp> Specify the location of the \
Melt pre-processor";
  "-P", Arg.String add_plugin_include, "<dir> Look for plugins in <dir> \
(this option is passed to the Melt pre-processor)";
  "-I", Arg.String add_include, "<dir> Look for libraries in <dir> \
(this option is passed to the OCaml compiler)";

  "-classic-display", Arg.Set classic_display,
  " Call Ocamlbuild with -classic-display (do not work with Mlpost)";
  "-no-mlpost", Arg.Clear mlpost, " Do not call mlpost, use ocamlbuild instead \
(or ocamlc if -no-ocamlbuild)";
  "-no-ocamlbuild", Arg.Clear ocamlbuild, " Do not use Ocamlbuild";
  "-no-final", Arg.Clear final, " Do not produce the PS or the PDF";
  "-no-link", Arg.Clear link, " Do not create a symbolic link to the PS or PDF";

  "-byte", Arg.Clear native, " Compile to bytecode instead of native code";

  "-dvi", Arg.Set dvi, " Produce a DVI instead of a PS";
  "-pdf", Arg.Set pdf, " Produce a PDF instead of a PS";
  "-quiet", Arg.Set quiet, " Be quiet";
  "-q", Arg.Set quiet, " Same as -quiet";
  "-continue", Arg.Set continue, " Continue on errors";
  "-k", Arg.Set continue, " Same as -continue";
  "-fake", Arg.Set fake, " Do not actually execute commands";
  "-n", Arg.Set fake, " Same as -fake";

  "-bibtex", Arg.Set bibtex, " Use BibTeX";
  "-fast", Arg.Set fast, " Do not call LaTeX again to get references right";

  "-melt-dir", Arg.Set_string melt_dir, "<dir> Change the named used for \
the _melt directory";

  "-clean", Arg.Set clean, " Remove the _melt directory and, if not -no-link, \
all symbolic links of the current directory linking into _melt \
(cleaning is done before anything else)";

  "-version", Arg.Unit Version.print, " Print version";
]
let anon s =
  main_file := s;
  Queue.add s files
let usage =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [options] [other_files] main_file\n
All [other_files] will be copied in the _melt directory. In particular, this \
allows you to use other modules or libraries.\n"

let cmd x = ksprintf begin fun s ->
  if not !quiet then printf "%s\n%!" s;
  if not !fake then
    let code = Sys.command s in
    if code <> 0 && not !continue then exit code
end x

let melt_to_ml f =
  let o = Filename.chop_extension f ^ ".ml" in
  cmd "%s%s -dir \"../\" -open Latex -open Melt %s -o %s" !meltpp
    !meltpp_plugin_includes f o;
  o

let libopt lib =
  let dot_cma = if !native then ".cmxa" else ".cma" in
  if !mlpost then
    " -ccopt " ^ if !ocamlbuild then "\"-lib " ^ lib ^ "\"" else lib ^ dot_cma
  else if !ocamlbuild then " -lib \"" ^ lib ^ "\"" else " "^lib^dot_cma

let ml_to_tex f =
  let bf = Filename.chop_extension f in
  let pdfo = if !pdf then " -pdf" else "" in
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
    let prelude_file = bf ^ ".tex" in
    if Sys.file_exists prelude_file then
      " -latex " ^ prelude_file
    else ""
  in
  if !mlpost then
    cmd "mlpost -v%s%s%s%s%s%s%s%s%s%s%s %s"
      mlpost_preludeo
      classicdisplayo
      mlpost_includes
      pdfo pdfeo ocamlbuildo nativeo
      strlibo latexlibo meltlibo nameeo f
  else if !ocamlbuild then
    cmd "ocamlbuild%s%s%s%s%s%s%s %s.%s --%s%s"
      classicdisplayo
      ocamlbuild_includes
      strlibo unixlibo latexlibo mlpostlibo meltlibo bf ext pdfo nameo
  else begin
    cmd "ocaml%s%s%s%s%s%s%s %s -o %s.%s"
      (if !native then "opt" else "c")
      ocamlc_includes
      strlibo unixlibo latexlibo mlpostlibo meltlibo f bf ext;
    cmd "./%s.%s%s%s" bf ext pdfo nameo
  end

let produce_tex f =
  if Filename.check_suffix f ".mlt" then begin
    let ml = melt_to_ml f in
    ml_to_tex ml
  end else if Filename.check_suffix f ".ml" then
    ml_to_tex f
  else if not (Filename.check_suffix f ".tex") then begin
    if not !quiet then
      Printf.printf "Warning: don't know what to do with %s.\n%!" f
  end

let produce_final f =
  let bf = Filename.chop_extension f in
  let latex = if !pdf then "pdflatex" else "latex" in
  let latex =
    latex ^ " -interaction nonstopmode -file-line-error -halt-on-error"
  in
  let latop = " | latop > /dev/null" in

  cmd "%s %s%s" latex bf latop;
  if !bibtex then begin
    cmd "bibtex %s" bf;
    cmd "%s %s%s" latex bf latop
  end;
  if not !fast then
    cmd "%s %s%s" latex bf latop;

  if not !pdf && not !dvi then
    cmd "dvips %s" bf

let produce_link f =
  let bf = Filename.chop_extension f in
  let o =
    if !pdf then bf ^ ".pdf" else
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
  meltpp_plugin_includes := begin match !plugin_includes with
    | [] -> ""
    | l -> " " ^ String.concat " " (List.map (fun x -> "-P "^x) l)
  end;
  if !clean then do_clean ();
  if !main_file <> "" then begin
    let cwd = Sys.getcwd () in
    make_temp_dir ();
    Queue.iter produce_tex files;
    if !final then produce_final !main_file;
    chdir cwd;
    produce_link !main_file
  end
