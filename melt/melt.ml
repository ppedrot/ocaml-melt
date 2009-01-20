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

(* We don't use the Arg module in order not to force the user to handle
the -pdf and -name options *)

let pdf =
  let b = ref false in
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-pdf" then b := true
  done;
  !b

let rec no_extension f =
  try
    no_extension (Filename.chop_extension f)
  with Invalid_argument "Filename.chop_extension" -> f

let name =
  let name = ref (no_extension (Filename.basename Sys.argv.(0))) in
  for i = 1 to Array.length Sys.argv - 2 do
    if Sys.argv.(i) = "-name" then
      name := Sys.argv.(i+1)
  done;
  !name

let next_name =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    Printf.sprintf "%s-melt-figure%d" name !cnt

let latex l = Mlpost.Picture.tex (Latex.to_string l)

let mlpost ?(pdf = pdf) ?file f =
  let file = match file with
    | None -> next_name ()
    | Some file -> file
  in
  let ext = if pdf then ".mps" else ".1" in
  Mlpost.Metapost.emit file f;
  Latex.includegraphics (Latex.text (file ^ ext))

let emit ?(file = name ^ ".tex") x = Latex.to_file file x

module Verbatim = struct
  type latex_verbatim_function = string -> Latex.t
  type melt_verbatim_function =
      [ `V of string | `C of Latex.t | `M of Latex.t | `T of Latex.t ] list ->
        Latex.t

  let convert f l =
    Latex.concat begin List.map begin function
      | `V s -> f s
      | `C a | `M a | `T a -> a
    end l end

  let verbatim = convert Latex.Verbatim.verbatim
  let regexps x y = convert (Latex.Verbatim.regexps x y)
  let keywords ?apply x = convert (Latex.Verbatim.keywords ?apply x)
end
