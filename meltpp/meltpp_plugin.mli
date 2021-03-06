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

(** Meltpp plugin API. *)

(** {2 Verbatim functions} *)

(** About the expressive power of verbatim functions: there are three kinds of
verbatim functions.
- The meta verbatim functions are called by meltpp when printing the
pre-processed file. They have the greatest expressive power: they can parse
the quotation, and include the anti-quotations anywhere. This means that the
anti-quotations can be of any type (at the non-meta level).
- The complex verbatim functions are called by the program printed by meltpp.
Their expressive power differs for the anti-quotations, which must all be of
the same type.
- The simple verbatim functions just take strings. They don't care about
anti-quotations. A simple verbatim function is an instance of a complex verbatim
function which applies the simple function to all items and concatenates
the results, anti-quotation being inserted as-it. *)

type verbatim_item = [
| `V of string
| `C of Format.formatter -> unit -> unit
| `M of Format.formatter -> unit -> unit
| `T of Format.formatter -> unit -> unit
]
  (** A verbatim item is either a verbatim string or an anti-quotation.
Anti-quotations can be code anti-quotations ([`C]),
math anti-quotations ([`M]) or text anti-quotations ([`T]).
Applying the anti-quotation will print its piece of code, with no extra
parenthesis. It can be used with the ["%a"] formatter. *)

type verbatim_function = Format.formatter -> verbatim_item list -> unit
  (** A verbatim function takes an output channel and a list of verbatim items.
It should print some piece of code on the output channel, corresponding to
the translation of the verbatim items. *)

val declare_verbatim_function: string -> verbatim_function -> unit
  (** [f "x" x] declare the verbatim_function [x] of name ["x"]. The function
can then be used as a verbatim mode. *)

val verbatim_complex: string -> verbatim_function
  (** [verbatim_complex "f"] is a verbatim function which prints a piece of code
which will apply [f] to the [verbatim_item list], which is printed as an
expression of type
[[ `V of string | `C of 'a | `M of Latex.t | `T of Latex.t ] list]. *)

val verbatim_simple: string -> verbatim_function
  (** [verbatim_simple "f"] is a verbatim function which prints a piece of code
which will apply [f] to all quotations parts, keeping the anti-quotations
as it. The default verbatim function is actually
[verbatim_simple "Latex.Verbatim.verbatim"]. *)

(** {2 Miscellaneous} *)

val list_insert: 'a -> 'a list -> 'a list
  (** Insert a value between each element of a list:
[list_insert x [e1; ...; en]] returns [[e1; x; e2; x; ...; x; en]].
In particular, if [n <= 1] the list is left unchanged. *)

val list_iter_concat: Format.formatter -> (Format.formatter -> 'a -> unit) ->
  'a list -> unit
  (** [list_iter_concat fmt f l]: [f] is supposed to print each item of [l]
as a piece of code of type [Latex.t]. Concatenations are inserted between each
item to produce a piece of code of type [Latex.t]. *)

val escape_except_newline: string -> string
  (** Escape special Ocaml characters, except "\n". This allows a string to
be printed in the source code without changing the line numbers. *)
