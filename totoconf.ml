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

let interactive = ref false
let config_file = ref "Config"
let debug = ref false

let speclist = [
  "-c", Arg.Set_string config_file, "<file> Configuration file";
  "-i", Arg.Set interactive, " Interactive mode";
  "-debug", Arg.Set debug, " Debug mode (verbose)";
]
let anon_fun x = raise (Arg.Bad (x^": unknown option"))
let usage_msg =
  let name =
    let name = Filename.basename Sys.executable_name in
    if String.sub (String.lowercase name) 0 5 = "ocaml" then
      "configure.ml"
    else name
  in
  sprintf "ocaml %s [options]" name

(**************************************************************************)
(*                          Errors and Warnings                           *)
(**************************************************************************)

let echo x =
  ksprintf (fun s -> printf "%s\n" s) x

let debug x =
  ksprintf (fun s -> if !debug then printf "Debug: %s\n%!" s) x

let error x =
  ksprintf (fun s -> printf "%!"; eprintf "Error: %s\n%!" s; exit 1) x

let warnings = ref []

let warning x =
  ksprintf (fun s -> warnings := s :: !warnings) x

let success () =
  printf "%!";
  eprintf "%!";
  List.iter (eprintf "Warning: %s\n%!") (List.rev !warnings);
  printf "Configuration successful";
  match List.length !warnings with
    | 0 -> printf ".\n%!"
    | 1 -> printf " (1 warning).\n%!"
    | c -> printf " (%d warnings).\n%!" c

(**************************************************************************)
(*                                                                        *)
(**************************************************************************)

exception Exec_error of int

let exec_line cmd args =
  let c = String.concat " " (cmd::args) in
  let tmp = Filename.temp_file "configure_exec_line" ".out" in
  match Sys.command (c ^ " > " ^ tmp) with
    | 0 -> input_line (open_in tmp)
    | n -> raise (Exec_error n)

let which file =
  try
    exec_line "which" [file]
  with Exec_error _ ->
    raise Not_found

let guess_bins files () =
  let files =
    List.fold_left begin fun acc file ->
      try
        which file :: acc
      with Not_found ->
        debug "Not found in PATH: %s" file;
        acc
    end [] files
  in
  List.rev files

(**************************************************************************)
(*                              String Utils                              *)
(**************************************************************************)

module Str = struct
  let last_word s =
    try
      let i = String.rindex s ' ' in
      String.sub s (i+1) (String.length s - i - 1)
    with Not_found ->
      s

  let replace_char f c b =
    let f = String.copy f in
    for i = 0 to String.length f - 1 do
      if f.[i] = c then f.[i] <- b
    done;
    f
end

(**************************************************************************)
(*                            Version Parsing                             *)
(**************************************************************************)

module Version = struct
  type t = int list * string

  exception Uncomparable_versions of t * t

  let parse s =
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

  let rec compare_vnums a b = match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1::r1, x2::r2 ->
	match x1 - x2 with
	  | 0 -> compare_vnums r1 r2
	  | c -> c

  let prefix s1 s2 =
    let l1 = String.length s1 in
    String.length s2 >= l1 &&
      String.sub s2 0 l1 = s1

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

  let compare a b = compare (parse a) (parse b)
  let eq a b = compare a b = 0
  let ne a b = compare a b <> 0
  let le a b = compare a b <= 0
  let lt a b = compare a b < 0
  let ge a b = compare a b >= 0
  let gt a b = compare a b > 0
end

(**************************************************************************)
(*                         Previous Configuration                         *)
(**************************************************************************)

let parse_config file =
  let trim s =
    let is_blank = function
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false
    in
    let len = String.length s in
    let rec left n = if n < len && is_blank s.[n] then left (n+1) else n in
    let rec right n = if n >= 0 && is_blank s.[n] then right (n-1) else n in
    let left = left 0 and right = right (len-1) in
    if left < right then String.sub s left (right-left+1) else ""
  in
  let parse_config_line acc line =
    try
      let i = String.index line '=' in
      let left = String.sub line 0 i in
      let right = String.sub line (i + 1) (String.length line - i - 1) in
      (String.uppercase (trim left), trim right) :: acc
    with Not_found -> acc
  in
  let f = open_in file in
  let lines = ref [] in
  try
    while true do
      lines := input_line f :: !lines
    done;
    assert false
  with End_of_file ->
    close_in f;
    List.fold_left parse_config_line [] !lines

let config = ref []

let old_var x =
  try
    Some (List.assoc (String.uppercase x) !config)
  with Not_found ->
    None

(**************************************************************************)
(*                               Variables                                *)
(**************************************************************************)

type internal_var = {
  iv_name: string;
  iv_print: Format.formatter -> unit -> unit;
  iv_query: string option;
}

type 'a var = {
  name: string;
  value: 'a;
}

let (!!) x = x.value

let vars = ref []

let var_exists name =
  let name = String.uppercase name in
  List.exists (fun v -> String.uppercase v.iv_name = name) !vars

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t option
end

module type VAR = sig
  type data
  val make:
    ?query: string ->
    ?check: (data -> bool) ->
    ?guess: (unit -> data list) ->
    ?fail: (unit -> data) ->
    string -> data var
  val umake:
    ?query: string ->
    ?check: (data -> bool) ->
    ?guess: (unit -> data list) ->
    ?fail: (unit -> data) ->
    string -> unit
  val simple: string -> data -> data var
  val usimple: string -> data -> unit
  val get: data var -> data
  val print: Format.formatter -> data var -> unit
end

module Var(T: STRINGABLE) = struct
  type data = T.t

  let check _ = true

  let guess () = []

  let print fmt x =
    fprintf fmt "%s = %s\n" x.name (T.to_string x.value)

  let rec find_guess check = function
    | [] -> None
    | x::rem ->
        debug "Guess: %s" (T.to_string x);
        if check x then Some x else find_guess check rem

  let make ?query ?(check = check) ?(guess = guess) ?fail name =
    if var_exists name then
      warning "Variable %s has already been defined." name;

    let check_option = function
      | None -> false
      | Some x -> check x
    in

    (* Query the user until he gives a valid value or the default value. *)
    let do_query query def =
      let def_str =
        match def with
          | None -> ""
          | Some x -> sprintf " [%s]" (T.to_string x)
      in
      let rec ask () =
        printf "%s%s: %!" query def_str;
        let l = read_line () in
        if l = "" then def else
          let value = T.of_string l in
          if check_option value then
            value
          else begin
            printf "Invalid value: %s.\n%!" l;
            ask ()
          end
      in
      ask ()
    in

    (* Old value. *)
    let value =
      match old_var name with
        | None -> None
        | Some x ->
            debug "%s: Found old value = %s" name x;
            T.of_string x
    in

    (* Check or guess. *)
    let value =
      if check_option value then
        value
      else begin
        debug "Guessing for %s" name;
        find_guess check (guess ())
      end
    in

    (* User interaction. *)
    let value =
      match query with
        | Some query when !interactive ->
            do_query query value
        | _ ->
            value
    in

    (* Final check. *)
    let value =
      match value, fail, query with
        | None, None, None ->
            error "%s: Not found" name
        | None, None, Some query ->
            error "%s: Not found" query
        | None, Some fail, _ ->
            fail ()
        | Some value, _, _ ->
            value
    in

    (* Notify the user. *)
    begin
      match query with
        | Some query when not !interactive ->
            printf "%s: %s\n" query (T.to_string value)
        | _ ->
            ()
    end;

    let var = {
      name = name;
      value = value;
    } in

    (* Register the variable. *)
    let internal = {
      iv_name = name;
      iv_print = (fun fmt () -> print fmt var);
      iv_query = query;
    } in
    vars := internal :: !vars;

    var

  let umake ?query ?check ?guess ?fail name =
    ignore (make ?query ?check ?guess ?fail name)

  let simple name value =
    make ~check: (fun _ -> false) ~fail: (fun () -> value) name

  let usimple name value =
    ignore (simple name value)

  let get x = x.value
end

module SVar = Var(struct
                    type t = string
                    let of_string x = Some x
                    let to_string x = x
                  end)
module BVar = Var(struct
                    type t = bool
                    let of_string x = Some (String.uppercase x = "YES")
                    let to_string = function
                      | true -> "YES"
                      | false -> "NO"
                  end)
module IVar = Var(struct
                    type t = int
                    let of_string x =
                      try Some (int_of_string x)
                      with Failure "int_of_string" -> None
                    let to_string = string_of_int
                  end)
module FVar = Var(struct
                    type t = float
                    let of_string x =
                      try Some (float_of_string x)
                      with Failure "float_of_string" -> None
                    let to_string = string_of_float
                  end)

(**************************************************************************)
(*                            Init and Finish                             *)
(**************************************************************************)

let init ?(file = "Config") ?(spec = []) () =
  config_file := file;
  Arg.parse (Arg.align (spec @ speclist)) anon_fun usage_msg;
  config := (try parse_config !config_file with _ -> [])

let finish () =
  let out =
    try
      open_out !config_file
    with Sys_error s ->
      error "Cannot write to file: %s" !config_file
  in
  let fmt = formatter_of_out_channel out in
  let sec, prim =
    List.partition (fun x -> x.iv_query = None) (List.rev !vars) in
  fprintf fmt "# This file has been automatically generated.
# After any modification, you should run the configuration tool again.\n\n";
  List.iter begin fun var ->
    let desc = match var.iv_query with
      | Some x -> x
      | None -> assert false
    in
    fprintf fmt "# %s\n%a\n" desc var.iv_print ()
  end prim;
  if sec <> [] then
    fprintf fmt
      "# The following variables are not supposed to be edited by hand.\n";
  List.iter (fun var -> fprintf fmt "%a" var.iv_print ()) sec;
  fprintf fmt "%!";
  close_out out;
  success ()
