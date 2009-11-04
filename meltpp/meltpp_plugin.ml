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

type verbatim_item = Plugin_private.verbatim_item
type verbatim_function = Plugin_private.verbatim_function

let declare_verbatim_function =
  Hashtbl.add Plugin_private.verbatim_functions

let rec list_insert acc x = function
  | a::r -> list_insert (a::x::acc) x r
  | [] -> match List.rev acc with
      | [] -> []
      | _::r -> r
let list_insert x = list_insert [] x

let list_iter_concat fmt f = function
  | [] ->
      fprintf fmt "(text \"\")"
  | l ->
      let l = List.map (fun x -> `I x) l in
      let l = list_insert `C l in
      fprintf fmt "(";
      List.iter begin function
        | `I x -> fprintf fmt "(%a)" f x
        | `C -> fprintf fmt "^^"
      end l;
      fprintf fmt ")"

let escape_except_newline s =
  let l = Str.split_delim (Str.regexp "\n") s in
  let l = List.map String.escaped l in
  String.concat "\n" l

let verbatim_complex name: verbatim_function = fun f l ->
  let l = list_insert `I (l :> [ verbatim_item | `I ] list) in
  fprintf f "(%s [" name;
  List.iter begin function
    | `V s -> fprintf f "`V \"%s\"" (escape_except_newline s)
    | `C a -> fprintf f "`C(%a)" a ()
    | `M a -> fprintf f "`M(%a)" a ()
    | `T a -> fprintf f "`T(%a)" a ()
    | `I -> fprintf f "; "
  end l;
  fprintf f "])"

let verbatim_simple name: verbatim_function = fun f l ->
  list_iter_concat f begin fun f -> function
    | `V s -> fprintf f "(%s \"%s\")" name (escape_except_newline s)
    | `C a | `M a | `T a -> fprintf f "(%a)" a ()
  end l
