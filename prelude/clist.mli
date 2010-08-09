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
