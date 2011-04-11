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

open Melt_common

let print_tex_dependencies basefile depends =
  if print_depends then begin
    let file = basefile ^ ".depends" in
    (* We sort the dependencies so the order of the files does not effect
       the .depends file. *)
    let depends = List.sort String.compare depends in
    let ch = open_out file in
    List.iter
      (fun dep ->
        if Sys.file_exists dep then
          let hash = Digest.file dep in
          Digest.output ch hash
        else
          ())
      depends;
    close_out ch
  end

let emit ?(file = name ^ ".tex") x =
  let env = Latex.to_file file x in
  print_tex_dependencies file (Latex.get_in_env tex_dependencies env)

let rec list_split_when f ?(acc = []) = function
  | [] -> raise Not_found
  | x :: r as l ->
      if f x then List.rev acc, l else
        let acc = x :: acc in
        list_split_when f ~acc r

module Verbatim = struct
  type melt_verbatim_string =
      [ `V of string | `C of Latex.t | `M of Latex.t | `T of Latex.t ] list
  type latex_verbatim_function = string -> Latex.t
  type melt_verbatim_function = melt_verbatim_string -> Latex.t

  let convert f l =
    Latex.concat begin List.map begin function
      | `V s -> f s
      | `C a -> a
      | `M a -> Latex.mode Latex.M a
      | `T a -> Latex.mode Latex.T a
    end l end

  let rec split_verbs_begin first = function
    | [] -> first, []
    | (`V v)::rem -> split_verbs_begin (first^v) rem
    | x -> first, x
  let split_verbs_begin = split_verbs_begin ""

  let rec split_verbs_end last = function
    | [] -> [], last
    | (`V v)::rem -> split_verbs_end (v^last) rem
    | x -> List.rev x, last
  let split_verbs_end l = split_verbs_end "" (List.rev l)

  let split_verbs l =
    let first, rem = split_verbs_begin l in
    let middle, last = split_verbs_end rem in
    first, middle, last

  let trim chars l =
    let first, middle, last = split_verbs l in
    let first =
      if middle = [] then
        `V (Latex.Verbatim.trim chars first)
      else
        `V (Latex.Verbatim.trim_begin chars first)
    in
    let last = `V (Latex.Verbatim.trim_end chars last) in
    first :: middle @ [last]

  let split_lines verb: melt_verbatim_string list =
    let rec f = function
      | `V s ->
          List.map
            (function Str.Text s -> `V s | Str.Delim s -> `V "\n")
            (Str.full_split (Str.regexp_string "\n") s)
      | x -> [x]
    in
    let rec split verb =
      try
        let a, b = list_split_when (fun x -> x = `V "\n") verb in
        a :: (split (List.tl b))
      with Not_found ->
        [verb]
    in
    split (List.flatten (List.map f verb))

  let verbatim = convert Latex.Verbatim.verbatim
  let regexps x y = convert (Latex.Verbatim.regexps x y)
  let keywords ?apply x = convert (Latex.Verbatim.keywords ?apply x)
  let pseudocode ?(trim = trim ['\n']) ?id_regexp
      ?kw_apply ?id_apply ?rem_apply ?keywords ?symbols ?keyword_symbols
      ?underscore s =
    let s = trim s in
    convert (Latex.Verbatim.pseudocode ~trim: (fun x -> x) ?id_regexp
               ?kw_apply ?id_apply ?rem_apply ?keywords ?symbols
               ?keyword_symbols ?underscore) s
end

include Mlpost_specific

module Arg =
struct
  open Latex

  let parameter_present name =
    let rec aux i =
      if i = Array.length Sys.argv
      then false
      else (aux (i+1)) || ( Sys.argv.(i) = ( "-" ^ (to_string name)))
    in
    aux 1

  let parameter_value default f name =
    let rec aux i =
      if i = Array.length Sys.argv - 1
      then default
      else
	if ( Sys.argv.(i) = ( "-" ^ (to_string name)))
	then f (Sys.argv.(i + 1))
	else aux (i+1)
    in
    aux 1

  let bool name = parameter_present name

  let int ?(default=0) name = parameter_value default int_of_string name

  let float ?(default=0.) name = parameter_value default float_of_string name

  let text ?(default=(text "")) name = parameter_value default (fun x -> text x) name



  let mode = Melt_common.mode
end
