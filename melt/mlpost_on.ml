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

module type Signature = sig
  (** Part of [Melt] which uses [Mlpost]. *)

  val compiled_with_mlpost: bool
    (** The value of [compiled_with_mlpost] is [true]. *)

  (** Convert some LaTeX into a picture. *)
  val picture_of_latex: Latex.t -> Mlpost.Picture.t

  (** Emit a figure to use it in a LaTeX document. *)
  val mlpost: ?mode: Melt_common.mode ->
    ?file: string -> Mlpost.Command.t -> Latex.t
  (**  The default  value  of [~mode]  is  the mode specified via the
command line (either to the melt tool or directly to the executable).

The [~file] parameter may be used if you want to specify the file name
used  for the  figure Metapost  script. Otherwise,  a default  name is
chosen.  This default name is [base.melt.figureN.ext], where [base] is
the executable base name (can  be overriden with the [-name] option on
the  command line),  [N] is  the figure  index and  [ext] is  [mps] if
[-pdf] is [true] or [1] otherwise. *)

  module Beamer :
    sig
      include Latex.BEAMER

      val mlpost: ?only: Latex.Beamer.overlays_spec list -> 
        ?mode: Melt_common.mode ->
        ?file: string -> Mlpost.Command.t -> Latex.t

    end
end

let compiled_with_mlpost = true

open Melt_common

let picture_of_latex l =
  try
    Mlpost.Picture.tex (Latex.to_string l)
  with (Invalid_argument txt) ->
    Mlpost.Picture.tex
      ("Cannot compile Mlpost figure. Reason: " ^
          (Latex.to_string
             (Latex.texttt
                (Latex.Verbatim.verbatim txt))))

let mlpost_gen includegraphics ?(mode = mode) ?file f =
  let file = match file with
    | None -> next_name ()
    | Some file -> file
  in
  let ext = match mode with
    | `Pdf -> ".mps"
    | `Ps -> ".1"
    | `Cairo -> ".pdf"
    | `Mps -> ".mps"
  in
  let full_name = file ^ ext in
  Mlpost.Metapost.emit file f;
  Latex.concat
    [Latex.setf tex_dependencies
	(fun tex_dependencies -> full_name :: tex_dependencies);
     includegraphics (Latex.text full_name)]

let mlpost = mlpost_gen Latex.includegraphics

module Beamer =
struct
  include Latex.Beamer

  let mlpost ?only = mlpost_gen (fun x -> Latex.Beamer.includegraphics ?only x)
end
