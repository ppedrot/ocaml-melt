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

let interactive = ref false

let speclist = [
  "-i", Arg.Set interactive, "interactive mode";
]
let anon_fun x = raise (Arg.Bad (x^": unknown option"))
let usage_msg = "ocaml configure.ml [-i]"

let () = Arg.parse speclist anon_fun usage_msg

type yes_no =
  | Yes of string
  | No of string

module Version: sig
  type t = int list * string
  exception Uncomparable_versions of t * t
  val of_string: string -> t
  val to_string: t -> string
  val compare: t -> t -> int
  val max: t -> t -> t
end = struct
  type t = int list * string

  exception Uncomparable_versions of t * t

  let of_string s =
    let rec first_not_digit p =
      if p < String.length s then
	match s.[p] with
	  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' ->
	      first_not_digit (p + 1)
	  | _ -> p
      else raise Not_found
    in
    let num_part, suffix_part =
      try
	let i = first_not_digit 0 in
	String.sub s 0 i, String.sub s i (String.length s - i)
      with Not_found -> s, ""
    in
    let rec split s p =
      try
	let i = String.index_from s p '.' in
	String.sub s p (i-p) :: (split s (i+1))
      with Not_found ->
	if s = "" then [] else [String.sub s p (String.length s - p)]
    in
    List.map int_of_string (split num_part 0), suffix_part

  let to_string (v, s) =
    String.concat "." (List.map string_of_int v) ^ s

  let prefix s1 s2 =
    let l1 = String.length s1 in
    String.length s2 >= l1 &&
      String.sub s2 0 l1 = s1

  let rec compare_vnums a b = match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1::r1, x2::r2 ->
	match x1 - x2 with
	  | 0 -> compare_vnums r1 r2
	  | c -> c

  let compare (v1, s1) (v2, s2) =
    match compare_vnums v1 v2 with
      | 0 ->
	  begin match prefix s1 s2, prefix s2 s1 with
	    | true, true -> 0
	    | true, false -> -1
	    | false, true -> 1
	    | false, false ->
		raise (Uncomparable_versions(([], s1), ([], s2)))
	  end
      | c -> c

  let max v1 v2 = match compare v1 v2 with
    | 1 -> v2
    | _ -> v1
end

let error x =
  ksprintf (fun s -> flush stdout; eprintf "Error: %s\n%!" s; exit 1) x

let warnings = ref []

let warning x =
  ksprintf (fun s -> warnings := s :: !warnings) x

let success () =
  flush stdout;
  flush stderr;
  List.iter (eprintf "Warning: %s\n%!") (List.rev !warnings);
  printf "Configuration successful";
  match List.length !warnings with
    | 0 -> printf ".\n%!"
    | 1 -> printf " (1 warning).\n%!"
    | c -> printf " (%d warnings).\n%!" c

exception Exec_error of int

let exec_line cmd args =
  let c = String.concat " " (cmd::args) in
  let tmp = Filename.temp_file "configure_exec_line" ".out" in
  match Sys.command (c ^ " > " ^ tmp) with
    | 0 -> input_line (open_in tmp)
    | n -> raise (Exec_error n)

let find s =
  try Yes(exec_line "which" [s]) with Exec_error _ -> No s

let rec first ?(name = "") = function
  | [] -> No name
  | x::r -> match find x with
      | No _ -> first ~name: (if name = "" then x else name) r
      | y -> y

let query question default =
  if !interactive then begin
    printf "%s [%s]: " question default;
    let l = read_line () in
    if l = "" then default else l
  end else begin
    printf "%s: %s\n" question default;
    default
  end

let check_file f =
  if not (Sys.file_exists f) || Sys.is_directory f then
    error "Could not find %s." f

let require = function
  | Yes s -> s
  | No s -> error "Could not find %s." s

let yes_no = function Yes s | No s -> s

let dirname x =
  let r = Filename.dirname x in
  if x = "" || x.[String.length x - 1] <> '/' then r ^ "/" else r
(******************************************************************************)

let () =
  let best x = first ~name: x [x^".opt"; x] in
  let ocamlc = query "OCaml compiler" (yes_no (best "ocamlc"))  in
  check_file ocamlc;
  let best_ocaml x = best (dirname ocamlc ^ x) in
  let ocamlbuild = best_ocaml "ocamlbuild" in
  let ocaml = best_ocaml "ocaml" in
  let ocamlopt = best_ocaml "ocamlopt" in
  let ocamllex = require (best_ocaml "ocamllex") in
  let ocamlyacc = require (best_ocaml "ocamlyacc") in
  let ocamldoc = best_ocaml "ocamldoc" in

  let ocamlc_where = exec_line ocamlc ["-where"] in
  let ocamlfind = find "ocamlfind" in
  let libdir ?fail pkg cm =
    let result = match ocamlfind with
      | Yes ocamlfind ->
          begin try
            let dir = exec_line ocamlfind ["query"; pkg] in
            if Sys.file_exists (Filename.concat dir cm) then
              Some dir
            else
              None
          with Exec_error _ ->
            None
          end
      | No _ ->
          None
    in
    let result = match result with
      | Some dir -> dir
      | None ->
          if Sys.file_exists (Filename.concat ocamlc_where cm) then
            ocamlc_where
          else begin
            let dir = query ("Library directory ("^pkg^")") "" in
            match fail with
              | None ->
                  check_file (Filename.concat dir cm);
                  dir
              | Some fail ->
                  fail ()
          end
    in
    printf "Found %s in %s\n" pkg result;
    if result = ocamlc_where then "" else result
  in
  let libflags l =
    let l = List.filter (fun x -> x <> "") l in
    let l =
      List.map (fun x -> sprintf "-cflags -I,%s -lflags -I,%s" x x) l in
    String.concat " " l
  in
  let ocaml_includes l =
    let l = List.filter (fun x -> x <> "") l in
    let l = List.map (fun x -> sprintf "-I %s" x) l in
    String.concat " " l
  in

  let install_bin = query "Install directory (program binaries)"
    "/usr/local/bin" in
  let install_lib = query "Install directory (OCaml libraries)" ocamlc_where in
  let mlpost = ref true in
  let libdir_mlpost =
    try
      libdir
        ~fail: (fun () -> warning "Cannot find Mlpost"; raise Exit)
        "mlpost"
        "mlpost.cma"
    with Exit ->
      mlpost := false;
      ""
  in
  let libs = [
    libdir_mlpost;
    libdir "extlib" "extLib.cma";
    libdir "pop" "pop.cma";
  ] in

  let out = open_out "Config" in
  let var = fprintf out "%s = %s\n" in
  let ovar x = function No _ -> var x "NO" | Yes y -> var x y in
  let bvar x = function true -> var x "YES" | false -> var x "NO" in
  bvar "MLPOST" !mlpost;
  var "OCAMLFLAGS" (ocaml_includes libs);
  var "OCAMLBUILDFLAGS" (libflags libs);
  ovar "OCAMLBUILD" ocamlbuild;
  var "OCAMLC" ocamlc;
  ovar "OCAML" ocaml;
  ovar "OCAMLOPT" ocamlopt;
  var "OCAMLLEX" ocamllex;
  var "OCAMLYACC" ocamlyacc;
  ovar "OCAMLDOC" ocamldoc;
  var "INSTALLBIN" install_bin;
  var "INSTALLLIB" install_lib;

  success ()
