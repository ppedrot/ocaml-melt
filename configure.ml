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

open Format
#use "totoconf.ml"

let () =
  init ~file: "Config" ();

  let ocamlc = SVar.make
    ~query: "OCaml bytecode compiler"
    ~guess: (guess_bins ["ocamlc.opt"; "ocamlc"])
    "OCAMLC"
  in

  let ocaml_dir = Filename.dirname !!ocamlc in
  let ocaml_version = exec_line !!ocamlc ["-version"] in
  echo "OCaml version: %s" ocaml_version;
  let ocaml_where = exec_line !!ocamlc ["-where"] in

  let ocaml_var name =
    SVar.umake
      ~guess: (guess_bins [
                 Filename.concat ocaml_dir (name ^ ".opt");
                 Filename.concat ocaml_dir name;
                 name ^ ".opt";
                 name
               ])
      ~check: (fun s ->
                 let v = Str.last_word (exec_line s ["-version"]) in
                 if name <> "ocamlbuild" || Version.ge ocaml_version "3.11" then
                   if not (Version.eq ocaml_version v) then
                     warning "Version of %s (%s) do not match \
compiler version (%s)" s v ocaml_version;
                 true)
      (String.uppercase name)
  in

  ocaml_var "ocamlopt";
  ocaml_var "ocamlbuild";
  ocaml_var "ocaml";
  ocaml_var "ocamllex";
  ocaml_var "ocamlyacc";
  ocaml_var "ocamldoc";

  BVar.usimple "NATDYNLINK" (Version.ge ocaml_version "3.11");

  let cm_dir pkg cm = SVar.make
    ~guess: (fun s ->
               let l = [
                 Filename.concat ocaml_where pkg;
                 ocaml_where;
                 Filename.concat ocaml_dir pkg;
                 ocaml_dir;
               ] in
               try
                 exec_line "ocamlfind" ["query"; "mlpost"; "2> /dev/null"] :: l
               with Exec_error _ -> l)
    ~check: (fun s -> Sys.file_exists (Filename.concat s cm))
  in

  let mlpost_cm_dir =
    cm_dir "mlpost" "mlpost.cma"
      ~query: "Mlpost library directory"
      ~fail: (fun () -> warning "Mlpost not found"; "") "MLPOST" in

  let mlpost = !!mlpost_cm_dir <> "" in
  BVar.usimple "MLPOST" mlpost;
  SVar.usimple "MLPOSTSPECIFIC"
    (if mlpost then "melt/mlpost_on.ml" else "melt/mlpost_off.ml");

  SVar.umake
    ~query: "Install directory (tool binaries)"
    ~guess: (fun () -> ["/usr/local/bin"])
    "INSTALLBIN";

  SVar.umake
    ~query: "Install directory (OCaml libraries)"
    ~guess: (fun () -> [ocaml_where])
    "INSTALLLIB";

  let ocaml_includes l =
    let l = List.filter (fun s -> s <> "" && s <> ocaml_where) l in
    let l = List.map (sprintf "-I %s") l in
    String.concat " " l
  in

  let ocaml_includes =
    SVar.simple "OCAMLINCLUDES" (ocaml_includes [!!mlpost_cm_dir]) in

  let ocamlbuild_flags l =
    let l = String.concat " " l in
    let l = Str.replace_char l ' ' ',' in
    if l <> "" then sprintf "-cflags %s -lflags %s" l l else ""
  in

  SVar.usimple "OCAMLBUILDFLAGS" (ocamlbuild_flags [!!ocaml_includes]);

  finish ()
