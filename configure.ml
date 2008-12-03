(*******************************************************************************)
open Printf

module Version: sig
  type t = int list * string
  exception Uncomparable_versions of t * t
  val of_string: string -> t
  val to_string: t -> string
  val compare: t -> t -> int
end = struct
  type t = int list * string

  exception Uncomparable_versions of t * t

  let of_string s =
    let p = ref 0 in
    let v = ref [] in
    begin try while true do
      let i = String.index_from s !p '.' in
      v := int_of_string (String.sub s !p (i - !p)) :: !v;
      p := i + 1
    done with Not_found | Failure "int_of_string" -> () end;
    List.rev !v, String.sub s !p (String.length s - !p)

  let to_string (v, s) =
    String.concat "." (List.map string_of_int v) ^ s

  let prefix s1 s2 =
    let l1 = String.length s1 in
    String.length s2 >= l1 &&
      String.sub s2 0 l1 = s1

  let compare (v1, s1) (v2, s2) =
    match List.fold_left2 begin function
      | 0 -> compare
      | c -> fun _ _ -> c
    end 0 v1 v2 with
      | 0 ->
          begin match prefix s1 s2, prefix s2 s1 with
            | true, true -> 0
            | true, false -> -1
            | false, true -> 1
            | false, false ->
                raise (Uncomparable_versions((v1, s1), (v2, s2)))
          end
      | c -> c
end

type executable = {
  name: string;
  version: Version.t;
}

let var x y = printf "%s = %s\n" x y
(*******************************************************************************)

(*let () =
  var "OCAMLC" ocamlc.name;
  var "OCAMLOPT" ocamlopt.name;
  var "OCAMLLEX" ocamllex.name;
  var "OCAMLYACC" ocamlyacc.name;
  var "OCAMLDOC" ocamldoc.name;
  var "BIN" bin;
  var "OCAMLLIB" 
*)
