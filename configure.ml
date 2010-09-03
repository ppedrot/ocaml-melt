
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
  init
    ~file: "Config"
    ~spec: [
      force "OCAMLC" "<path> OCaml bytecode compiler";
      force "MLPOST" "<path> Mlpost library directory";
      force "INSTALLBIN" "<path> Install directory (tool binaries)";
      force "INSTALLLIB" "<path> Install directory (OCaml libraries)";
      force "INSTALLMAN" "<path> Install directory (man pages)";
    ]
    ();

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
                 exec_line "ocamlfind" ["query"; pkg; "2> /dev/null"] :: l
               with Exec_error _ -> l)
    ~check: (fun s -> Sys.file_exists (Filename.concat s cm))
  in

  let mlpost_cm_dir =
    cm_dir "mlpost" "mlpost.cma"
      ~query: "Mlpost library directory"
      ~fail: (fun () -> warning "Mlpost not found"; "") "MLPOSTLIBDIR" in

  let check_mlpost_version () =
    try
      let v = exec_line (which "mlpost") ["-version"; "2> /dev/null"] in
      if Version.ge v "0.6" || v = "current" then begin
        echo "Mlpost version: %s" v;
        true
      end else begin
        echo "Mlpost version too old (%s)" v;
        false
      end
    with
      | Not_found ->
          warning "Mlpost tool not found.";
          false
      | Exec_error 2 ->
          warning "Mlpost version too old (<0.6).";
          false
  in

  let mlpost = !!mlpost_cm_dir <> "" && check_mlpost_version () in
  BVar.usimple "MLPOST" mlpost;
  SVar.usimple "MLPOSTSPECIFIC"
    (if mlpost then "melt/mlpost_on.ml" else "melt/mlpost_off.ml");

  let mlpost_with_cairo =
    mlpost &&
      try ignore (exec_line "mlpost" ["-cairo"]); true
      with Exec_error _ -> false
  in
  BVar.usimple "MLPOSTCAIRO" mlpost_with_cairo;

  let cm_dir_if_mlpost_with_cairo pkg cm name var =
    if mlpost_with_cairo then
      cm_dir pkg cm
        ~query: (name^" library directory")
        ~fail: (fun () -> warning "%s not found" name; "")
        var
    else
      cm_dir pkg cm ~fail: (fun () -> "") var
  in

  let bitstring_cm_dir =
    cm_dir_if_mlpost_with_cairo
      "bitstring" "bitstring.cma" "Bitstring" "BITSTRINGLIBDIR"
  in

  let cairo_cm_dir =
    cm_dir_if_mlpost_with_cairo
      "cairo" "cairo.cma" "Cairo" "CAIROLIBDIR"
  in

  SVar.umake
    ~query: "Install directory (tool binaries)"
    ~guess: (fun () -> ["/usr/local/bin"])
    "INSTALLBIN";

  SVar.umake
    ~query: "Install directory (OCaml libraries)"
    ~guess: (fun () -> let l = [Filename.concat ocaml_where "melt"] in
             try
               let dir = exec_line "ocamlfind" 
                 ["printconf"; "destdir"; "2> /dev/null"] in
               (Filename.concat dir "melt"):: l
             with Exec_error _ -> l
            )
    "INSTALLLIB";

  SVar.umake
    ~query: "Install directory (man pages)"
    ~guess: (fun () -> ["/usr/local/share/man/man1"])
    "INSTALLMAN";

  let ocaml_includes l =
    let l = List.filter (fun s -> s <> "" && s <> ocaml_where) l in
    let l = List.map (sprintf "-I %s") l in
    String.concat " " l
  in

  let ocaml_includes =
    let includes =
      if mlpost then [
        !!mlpost_cm_dir;
        !!bitstring_cm_dir;
        !!cairo_cm_dir
      ] else []
    in
    SVar.simple "OCAMLINCLUDES" (ocaml_includes includes)
  in

  let ocamlbuild_flags l =
    let l = String.concat " " l in
    let l = Str.replace_char l ' ' ',' in
    if l <> "" then sprintf "-cflags %s -lflags %s" l l else ""
  in

  SVar.usimple "OCAMLBUILDFLAGS" (ocamlbuild_flags [!!ocaml_includes]);

  finish ()
