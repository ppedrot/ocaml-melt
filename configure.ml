(******************************************************************************)
open Printf

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

let best _ = assert false

let query ?(default = "") question = assert false

let require _ = assert false

let exec _ _ = assert false

let check_file _ = assert false
(******************************************************************************)

let () =
  let bestopt x = best [x^".opt"; x] in
  let ocamlc = query ?default: (bestopt "ocamlc") "OCaml compiler" in
  check_file ocamlc;
  let best_ocaml x = bestopt (Filename.dirname ocamlc ^ x) in
  let ocamlopt = best_ocaml "ocamlopt" in
  let ocamllex = require (best_ocaml "ocamllex") in
  let ocamlyacc = require (best_ocaml "ocamlyacc") in
  let ocamldoc = best_ocaml "ocamldoc" in
  let bin = query ~default: "/usr/local/bin" "Install directory (programs)" in
  let ocaml_lib = query ~default: (exec ocamlc ["-where"])
    "Install directory (OCaml libraries)" in

  let ocaml_version = Version.of_string (exec ocamlc ["-version"]) in

  let out = open_out "Config" in
  let var = fprintf out "%s = %s\n" in
  let ovar x = function None -> () | Some y -> var x y in
  var "OCAMLC" ocamlc;
  ovar "OCAMLOPT" ocamlopt;
  var "OCAMLLEX" ocamllex;
  var "OCAMLYACC" ocamlyacc;
  ovar "OCAMLDOC" ocamldoc;
  var "BIN" bin;
  var "OCAMLLIB" ocaml_lib;

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
Install directory (OCaml libraries): %s"
    (Version.to_string ocaml_version) ocamlc ocamlopt ocamllex ocamlyacc
    ocamldoc bin ocaml_lib
