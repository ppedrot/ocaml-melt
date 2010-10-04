(**************************************************************************)
(* Copyright (c) 2010, Arnaud Spiwack                                     *)
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

(*** Lists with constant time concatenation ***)

type +'a t

(** [Empty] is raised by [head] and [tail] if the list
    is empty. *)
exception Empty

(** [empty] is the empty list. *)
val empty : 'a t
(** [is_empty l] returns [true] if and only if
    [l] is the empty list. *)
val is_empty : 'a t -> bool

(** [cons x l] adds [x] in front of [l]. *)
val cons : 'a -> 'a t -> 'a t
(** [singleton x] is the list comprised only of [x]. *)
val singleton : 'a -> 'a t
(** [app l r] appends [r] at the end of [l]. *)
val app : 'a t -> 'a t -> 'a t

(** [of_list l] returns the Clist.t equivalent to [l]. *)
val of_list : 'a list -> 'a t
(** [list_cons [x1;...;xn] l] returns [(cons x1 ... (cons xn l)  ...)]. *)
val list_cons : 'a list -> 'a t -> 'a t

(** first element of the list. *)
val head : 'a t -> 'a
(** the list without its first element. *)
val tail : 'a t -> 'a t


(** [fold_left f a [b1; ...; bn]] is
    [f (... (f (f a b1) b2) ...) bn]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** [list_concat [l1;l2;...;ln] returns the list
    app l1 (app l2 (app ... ln)). *)
val list_concat : 'a t list -> 'a t

(** [concat [l1;l2;...;ln] returns the list
    app l1 (app l2 (app ... ln)). *)
val concat : 'a t t -> 'a t

(** [iter f [x1;x2;...;xn]] is equivalent to
    [f x1; f x2; ...; f xn]. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [map f [x1;x2;...;xn]] is equivalent to
    [[f x1; f x2; ...; f xn]]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [forall p l] is true if [p] holds on every
    element of l. *)
val forall : ('a -> bool) -> 'a t -> bool
