(*** Persistent, constant time queues ***)

(* spiwack: a short comment on the choice of this particular implementation
   (physicist's queue from Okasaki's Purely Functional Datastructure).
   Melt is batch-compiled, hence we do not need the hassle of having
   a good worst-time analysis, amortised performances are quite sufficient.
   Physicist's queues are arguably the most simple kind of amortised queues,
   it requires less suspension than the banker's queues (which incidentally
   should hopefully mean that it has a lower overhead).
   I chose this implementation rather than a simpler and more efficient
   one in a (mostly) single threaded setting to cope with copying of pieces of
   [Latex.t].*)

(* For a detailed description of the implementation and its time analysis
    see Pysicist's queues from Okasaki's Purely Functional Datastructures. *)


type 'a t = {
  (* An already forced prefix of [front] from which values are peeked. *)
  working: 'a list ;
  (* Length of [front]. *)
  lenf: int ;
  (* Front of the queue. *)
  front: 'a list Lazy.t ;
  (* Length of [rear]. *)
  lenr: int ;
  (* Rear of the queue, in reversed order. *)
  rear: 'a list
}

exception Empty

let empty = {
  working = [] ;
  lenf = 0 ;
  front = Lazy.lazy_from_val [] ;
  lenr = 0 ;
  rear = []
}

(* An invariant is that [lenr] <= [lenf], hence if [lenf=0] then [lenr=0]. *)
let is_empty q = q.lenf = 0

(* After [ensure_working], [working] is empty if and only if [lenf=0]. *)
let ensure_working = function
  | { working=[] } as q -> { q with working = Lazy.force q.front }
  | q -> q

(* After [ensure_invariant], [working is empty if and only if [lenf=0] and
    [lenr]<=[lenf] *)
let ensure_invariant q =
  if q.lenr <= q.lenf then
    ensure_working q
  else
    let f = Lazy.force q.front in
    ensure_working {
      working = f ;
      lenf = q.lenf+q.lenr ;
      front = lazy (f@(List.rev q.rear)) ;
      lenr = 0 ;
      rear = []
    }

let push x q =
  ensure_invariant { q with lenr = q.lenr+1 ; rear = x::q.rear }

let peek = function
  | { working = [] } -> raise Empty
  | { working = x::_ } -> x

let pop = function
  | { working = [] } -> raise Empty
  | { working = _::w } as q ->
      ensure_invariant { q with 
			   working = w ;
			   lenf = q.lenf-1 ;
			   front = lazy (List.tl (Lazy.force q.front)) }

