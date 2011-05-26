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




(** Type of variables. Variables can be modified during the
    evaluation of a document. Variables can read current, past
    and future value, provided this is well-defined.
    Further documentation can be found in the {!Latex}
    module. *)
type 'a t

val make: ?eq:('a -> 'a -> bool) -> ?name:string ->
  ?printer:('a -> string) -> 'a -> 'a t
(** Declare a new variable.  The last argument is the default value of
    the variable.

    [eq] is the equality function on the type of the variable. Default is [=].

    [name] and [printer] are used to print information when the
    fixpoint calculation diverged. *)




(** A position in a document. A variable can read the value
    it took or will take at a given position. *)
type position

val position : ?name:string -> unit -> position



(** Type of environment giving values to variables. *)
type env
val empty : env

(** Meta-get on environment, expectingly not to be used
    in document, but rather by document processors.
    Retrieves the value of a variable at given position. *)
val get_in_env : ?position:position -> 'a t -> env -> 'a




(** Type of documents extended with variables. *)
type ('a,'b) with_var

val raw : 'a -> ('a,'b) with_var

val setf : 'x t -> ('x->'x) -> ('a,'b) with_var
val setf2 : 'x t -> 'y t -> ('x->'y->'y) -> ('a,'b) with_var
val set : 'x t -> 'x -> ('a,'b) with_var

val get : ?position:position -> 'x t -> ('x -> 'b) -> ('a,'b) with_var

val place : position -> ('a,'b) with_var

(** [content x] returns [Some y] if [x] is [raw y] and [None]
    otherwise. *)
val content : ('a,'b) with_var -> 'a option

(** [computer empty encapsulate recurse env x] computes a value of
    type ['r] where [empty] is a value of type ['r] (supposedly
    representing an empty document) used to interpret [set] and
    [place], [encapsulate] interprets [raw] documents and
    [recurse] evaluates right-hand sides of [get].

    Raises [Multiple_place p] if the position [p] is already
    placed in [env] and [place p] is evaluated.*)
exception Multiple_place of position
val compute : 'r -> (env->'a -> env*'r) -> (env->'b -> env*'r) -> env -> ('a,'b) with_var -> env * 'r


(** [fixpoint env iterations f x] iterates
    [fun e -> fst (f e x)] starting on [env] until a fixpoint
    is reached or [iterations] iterations have been done.
    In the latter case it fails with [Fixpoint_divergent log].
    In the former, let [e0] be the fixpoint, the return value
    is then [f e0 x].
    The "current position" is reset with every iteration, so
    that the fixpoint effectively computed is only about
    snapshots at positions.

    By default, the starting environement is {!empty}.

    The default value for [iterations] is given by
    {!set_default_fixpoint_iterations}. The starting value
    being 10. *)
exception Fixpoint_divergent of position list list
val fixpoint :
  ?env:env -> ?iterations:int -> (env -> 'a -> env * 'b) -> 'a -> env * 'b

val set_default_fixpoint_iterations : int -> unit
