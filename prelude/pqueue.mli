(*** Persistent, constant time queues ***)

type 'a t

(** Raised by operations which do not support empty queues as argument. *)
exception Empty

(** [is_empty q] returns [true] if and only if [q] is the empty queue. *)
val is_empty : 'a t -> bool

(** [push x q] adds [x] at the back of [q] *)
val push : 'a -> 'a t -> 'a t

(** [peek q] returns the value at the front of [q]. If [q] is empty, raises {!Empty}. *)
val peek : 'a t -> 'a

(** [pop q] removes the value at the front of [q]. If [q] is empty, raises {!Empty}. *)
val pop : 'a t -> 'a t
