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

(** Unification of [Mlpost], [Latex] and the preprocessor. *)

(** Emit a LaTeX file. *)
val emit: ?file: string -> Latex.t -> unit
  (**  This is  basically the  same than  [Latex.to_file]  except that
there is  a default file name  [base.tex] where [base]  is computed in
the same way than [base] in the [mlpost] function. *)

module Verbatim: sig
  (** Verbatim modes for the Melt pre-processor. *)

  (** These modes are the same as the ones in the [Latex.Verbatim] module,
except that they work with the Melt pre-processor. *)

  type melt_verbatim_string =
      [ `V of string | `C of Latex.t | `M of Latex.t | `T of Latex.t ] list
  type latex_verbatim_function = string -> Latex.t
  type melt_verbatim_function = melt_verbatim_string -> Latex.t

  val convert: latex_verbatim_function -> melt_verbatim_function
    (** Convert a verbatim function of the [Latex] module to a function
usable with the Melt pre-processor. The original function is applied to
each quotations; anti-quotations are left as it, and the resulting list
is concatenated. *)

  (** {2 Conversion of [Latex.Verbatim]} *)

  val trim: char list -> melt_verbatim_string -> melt_verbatim_string
    (** The [trim] function will only be applied at the beginning
of the first [`V] item and at the end of the last [`V] item. *)

  val split_lines: melt_verbatim_string -> melt_verbatim_string list

  val verbatim: melt_verbatim_function
  val regexps: (Str.regexp * (string -> Latex.t)) list -> (string -> Latex.t) ->
    melt_verbatim_function
  val keywords: ?apply: (Latex.t -> Latex.t) -> string list ->
    melt_verbatim_function
  val pseudocode : ?trim: (melt_verbatim_string -> melt_verbatim_string) ->
    ?id_regexp: Str.regexp ->
    ?kw_apply: (Latex.t -> Latex.t) ->
    ?id_apply: (Latex.t -> Latex.t) ->
    ?rem_apply: (string -> Latex.t) ->
    ?keywords: string list ->
    ?symbols: (string * Latex.t) list ->
    ?keyword_symbols: (string * Latex.t) list ->
    ?underscore: Str.regexp ->
    melt_verbatim_function
end

include Mlpost_specific.Signature
