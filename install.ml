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
#use "version.ml"

let bin = ref ""
let lib = ref ""
let build = ref ""
let uninstall = ref false
let fake = ref false

let speclist = [
  "-bin", Arg.Set_string bin, "Install directory (program binaries)";
  "-lib", Arg.Set_string lib, "Install directory (OCaml libraries)";
  "-build", Arg.Set_string build, "Base build directory";
  "-uninstall", Arg.Set uninstall, "Uninstall instead of install";
  "-fake", Arg.Set fake, "Do not execute commands, only print them.";
]
let anon_fun x = raise (Arg.Bad ("Unknown parameter: "^x))
let usage_msg = "ocaml install.ml -bin <dir> -lib <dir>"

let check sr =
  if !sr = "" then begin
    Arg.usage speclist usage_msg;
    exit 1
  end

let () =
  Arg.parse speclist anon_fun usage_msg;
  check bin;
  check lib

let script = Queue.create ()

let rec first name = function
  | [] ->
      eprintf "Warning: file %s has not been compiled.\n" name;
      raise Not_found
  | x::r ->
      let x = if !build = "" then x else !build ^ "/" ^ x in
      if Sys.file_exists x then x else first name r

let add_com com =
  Queue.add (`Com com) script

let add_fun s f =
  Queue.add (`Fun (s,f)) script

let mkdir dir =
    add_com (sprintf "mkdir -p %s" dir)

let install_file name =
  add_com (sprintf "install -D -m 644 %s %s/%s" name !lib name)

let install_lib l =
  let base = Filename.basename l in
  try
    let l = first base [l] in
    add_com (sprintf "install -D -m 644 %s %s/%s" l !lib base)
  with Not_found -> ()

let install_bin b final =
  try
    let b = first final b in
    add_com (sprintf "install -D %s %s/%s" b !bin final)
  with Not_found -> ()

let rm f =
  if Sys.file_exists f then
    add_com (sprintf "rm %s" f)
  else
    eprintf "Warning: file %s does not exist.\n" f

let rm_dir f =
  if Sys.file_exists f then
    add_com (sprintf "rmdir %s" f)
  else
    eprintf "Warning: dir %s does not exist or is not empty.\n" f

let uninstall_file file =
  rm (Filename.concat !lib file)

let uninstall_lib l =
  uninstall_file (Filename.basename l)

let uninstall_bin _ final =
  rm (Filename.concat (!bin) final)

let do_file = if !uninstall then uninstall_file else install_file
let do_lib = if !uninstall then uninstall_lib else install_lib
let do_bin = if !uninstall then uninstall_bin else install_bin

let check_code = function
  | 0 -> ()
  | n -> exit n

let execute = function
  | `Com cmd ->
      printf "%s\n%!" cmd;
      if not !fake then check_code (Sys.command cmd)
  | `Fun (s,f) ->
      printf "%s\n%!" s;
      if not !fake then f ()

let finish () =
  Queue.iter execute script

(**************************************************************************)
(*                            Ocamlfind META file                         *)
(**************************************************************************)

type meta = { description : string;
              version : string;
              requires : string list;
              archive : ([`Byte |`Native] list * string list) list;
              subpackage : (string * meta) list}

let create_meta ?(filename="META") meta =
  (* CREATE META FILE *)
  let rec print_meta o meta =
    fprintf o "description = \"%s\"\n" meta.description;
    fprintf o "version = \"%s\"\n" meta.version;
    (match meta.requires with
       | [] -> ()
       | _ -> fprintf o "requires = \"%s\"\n"
           (String.concat " " meta.requires);
    );
    List.iter (fun (preds,l) ->
                 fprintf o "archive(%s) = \"%s\"\n"
                   (String.concat ","
                      (List.map (function
                                   | `Byte -> "byte"
                                   | `Native -> "native") preds))
                   (String.concat " " l))
      meta.archive;
    List.iter (fun (s,m) ->
                 fprintf o "package \"%s\" (\n%a)\n" s print_meta m)
      meta.subpackage
  in
  add_fun (sprintf "META file created in %s" filename)
    (fun () ->
       let o = open_out filename in
       print_meta o meta;
       close_out o;)

let do_meta () =
  if !uninstall then
    uninstall_file "META"
  else
      let meta_latex =
        {version=full;
         description="Latex library for OCaml.";
         archive=[[`Byte],["latex.cma"];
                  [`Native],["latex.cmxa"]];
         requires=[];
         subpackage=[]} in
      let meta_melt = {version=full;
                       description=
          "Melt allows you to write Latex documents using OCaml.";
                       requires=["melt.latex"];
                       archive=[[`Byte],["melt.cma"];
                                [`Native],["melt.cmxa"]];
                       subpackage=["latex",meta_latex]} in
      create_meta
        ~filename:(Filename.concat !lib "META")
        meta_melt

let () =
  do_bin ["meltpp/main.native"; "meltpp/main.byte"] "meltpp";
  do_bin ["melt/tool.native"; "melt/tool.byte"] "melt";
  do_bin ["latop/latop.native"; "latop/latop.byte"] "latop";
  List.iter do_lib [
    "latex/latex.a";
    "latex/latex.cmi";
    "latex/latex.cma";
    "latex/latex.cmxa";
    "melt/melt.a";
    "melt/melt.cmi";
    "melt/melt.cma";
    "melt/melt.cmxa";
    "meltpp/meltpp_plugin.cmi"
  ];
  do_meta ();
  if !uninstall then rm_dir !lib;
  finish ()

