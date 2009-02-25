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

let install_lib l =
  let base = Filename.basename l in
  try
    let l = first base [l] in
    Queue.add (sprintf "install -D -m 644 %s %s/%s" l !lib base) script
  with Not_found -> ()

let install_bin b final =
  try
    let b = first final b in
    Queue.add (sprintf "install -D %s %s/%s" b !bin final) script
  with Not_found -> ()

let rm f =
  if Sys.file_exists f then
    Queue.add (sprintf "rm %s" f) script
  else
    eprintf "Warning: file %s does not exist.\n" f

let uninstall_lib l =
  rm (!lib ^ "/" ^ Filename.basename l)

let uninstall_bin _ final =
  rm (!bin ^ "/" ^ final)

let do_lib = if !uninstall then uninstall_lib else install_lib
let do_bin = if !uninstall then uninstall_bin else install_bin

let check_code = function
  | 0 -> ()
  | n -> exit n

let execute cmd =
  printf "%s\n%!" cmd;
  if not !fake then check_code (Sys.command cmd)

let finish () =
  Queue.iter execute script

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
  ];
  finish ()
