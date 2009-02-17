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

(** Tools to write configuration files. *)

val force: ?option: string -> string -> string -> Arg.key * Arg.spec * Arg.doc
  (** [force var doc] return a command line argument specification
usable in the [~spec] argument of [init]. Default value for [~option] is
["-"^var].
The user can use the [option] command line argument to force the value
of variable [var]. The value will be checked, and if the check fails,
the configuration script will exit. Else the value will be used for [var].
If variable [var] is not used by the configuration script, forcing its value
has no effect. *)

val init: ?file: string -> ?spec: (Arg.key * Arg.spec * Arg.doc) list ->
  unit -> unit
  (** Initialize the module. Should be called before anything else.
      - [~file]: configuration file name. Default is ["Config"]. Can be
overwritten using the [-c] option.
      - [~spec]: additional options. *)

val finish: unit -> unit
  (** Print warnings and write new configuration. *)

(** {2 Errors and Warnings} *)

val echo: ('a, unit, string, unit) format4 -> 'a
val debug: ('a, unit, string, unit) format4 -> 'a
val error: ('a, unit, string, unit) format4 -> 'a
val warning: ('a, unit, string, unit) format4 -> 'a

(** {2 Shell} *)

(** {3 Low-Level} *)

exception Exec_error of int

val exec_line: string -> string list -> string
  (** Execute a command with parameters. Return the first line of its standard
output. *)

val which: string -> string
  (** Find a file in [PATH]. Raise [Not_found] if the file cannot be found. *)

(** {3 High-Level} *)

val guess_bins: string list -> unit -> string list
  (** Apply [which] to each file in the list. Only keep existing files.
Used as a [~guess] argument of [VAR.make]. *)

(** {2 Version Parsing} *)

module Version: sig
  val compare: string -> string -> int
  val eq: string -> string -> bool
  val ne: string -> string -> bool
  val le: string -> string -> bool
  val lt: string -> string -> bool
  val ge: string -> string -> bool
  val gt: string -> string -> bool
end

(** {2 Options} *)

val interactive: bool ref
  (** Command line option [-i]: interactive mode. *)

val config_file: string ref
  (** Configuration file name. *)

(** {2 Variables} *)

type 'a var

val (!!): 'a var -> 'a

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t option
    (** This function shall return [None] if its parameter is not a valid
string representation of a [t]. *)
end

module type VAR = sig
  type data

  val make:
    ?query: string ->
    ?check: (data -> bool) ->
    ?guess: (unit -> data list) ->
    ?fail: (unit -> data) ->
    string -> data var
    (** Build a configuration variable. The old value (if any) is tried first.
Then guessed values are tried. The user may then be invited to confirm
or enter a new value. The variable will be printed when [finish] is executed.
        - [~query]: description of the variable. It is used when asking the user
to confirm the variable if interactive mode is set. 
The user can change the value, which is then checked.
With no description, there is no confirmation.
The description is also used to print the final value to the user.
        - [~check]: function used to check if a given value is correct for the
variable. Default always return [true].
        - [~guess]: function used to guess default values. The result is a list
of guesses which are tried in order until one of them passes [~check].
Default returns an empty list.
        - [~fail]: function used to return a default value. This default value
is not checked. This function may typically raise an exception, call [error]
or [warning]. Default value calls [error]. *)

  val umake:
    ?query: string ->
    ?check: (data -> bool) ->
    ?guess: (unit -> data list) ->
    ?fail: (unit -> data) ->
    string -> unit
    (** Same as [make] but does not return the variable. *)

  val simple: string -> data -> data var
    (** Build a simple variable with a name and a value. *)

  val usimple: string -> data -> unit
    (** Same as [simple] but does not return the variable. *)

  val get: data var -> data

  val print: Format.formatter -> data var -> unit
end

module Var: functor(T: STRINGABLE) -> VAR with type data = T.t

module SVar: VAR with type data = string
module BVar: VAR with type data = bool
module IVar: VAR with type data = int
module FVar: VAR with type data = float

(** {2 String Utils} *)

module Str: sig
  val last_word: string -> string
  val replace_char: string -> char -> char -> string
end
