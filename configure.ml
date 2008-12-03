(******************************************************************************)
open Printf

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

let error x = ksprintf (fun s -> eprintf "Error: %s\n%!" s; exit 1) x

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

let query ?(default = "") question =
  printf "%s [%s]: " question default;
  let l = read_line () in
  if l = "" then default else l

let check_file f =
  if not (Sys.file_exists f) || Sys.is_directory f then
    error "Could not find %s." f

let require = function
  | Yes s -> s
  | No s -> error "Could not find %s." s

let some_of_yes = function
  | Yes s -> Some s
  | No _ -> None

let dirname x =
  let r = Filename.dirname x in
  if x = "" || x.[String.length x - 1] <> '/' then r ^ "/" else r
(******************************************************************************)

let () =
  let best x = first ~name: x [x^".opt"; x] in
  let ocamlc = query ?default: (some_of_yes (best "ocamlc")) "OCaml compiler" in
  check_file ocamlc;
  let best_ocaml x = best (dirname ocamlc ^ x) in
  let ocamlopt = best_ocaml "ocamlopt" in
  let ocamllex = require (best_ocaml "ocamllex") in
  let ocamlyacc = require (best_ocaml "ocamlyacc") in
  let ocamldoc = best_ocaml "ocamldoc" in
  let install_bin = query ~default: "/usr/local/bin"
    "Install directory (programs)" in
  let install_lib = query ~default: (exec_line ocamlc ["-where"])
    "Install directory (OCaml libraries)" in

  let ocaml_version = Version.of_string
    (exec_line ocamlc ["-version"]) in

  let out = open_out "Config" in
  let var = fprintf out "%s = %s\n" in
  let ovar x = function No _ -> () | Yes y -> var x y in
  var "OCAMLC" ocamlc;
  ovar "OCAMLOPT" ocamlopt;
  var "OCAMLLEX" ocamllex;
  var "OCAMLYACC" ocamlyacc;
  ovar "OCAMLDOC" ocamldoc;
  var "INSTALLBIN" install_bin;
  var "INSTALLLIB" install_lib;

  let o = function No _ -> "NOT FOUND" | Yes x -> x in
  printf "
Summary:
--------
OCaml version: %s
ocamlc: %s
ocamlopt: %s
ocamllex: %s
ocamlyacc: %s
ocamldoc: %s
Install directory (programs): %s
Install directory (OCaml libraries): %s
"
    (Version.to_string ocaml_version) ocamlc (o ocamlopt) ocamllex ocamlyacc
    (o ocamldoc) install_bin install_lib
