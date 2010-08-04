(*** Lists with constant time concatenation ***)


(* The datastructure is taken from Okasaki's Purely Functional
   Datastructures. It is dubbed "Catenable Lists" and "Lists with
   efficient catenation". *)


exception Empty

type 'a t = E | Cons of 'a*('a t Lazy.t Pqueue.t)

let empty = E
let is_empty = function E -> true | _ -> false

let link x q s = Cons (x,Pqueue.push s q)
(* [force_link] is a variant of link called
   on deep lists: the only possible E is the outermost
   constructor *)
let force_link q s =
    match q with
    | E -> assert false
    | Cons (x,q) -> link x q s
let linkall q =
  let rec linkall q =
    let t = Lazy.force (Pqueue.peek q) in
    let q = Pqueue.pop q in
    if Pqueue.is_empty q then
      t
    else
      force_link t (lazy (linkall q))
  in
  if Pqueue.is_empty q then E else linkall q

let app l1 l2 = match l1,l2 with
  | _,E -> l1
  | E,_ -> l2
  | Cons (x,l1),_ -> link x l1 (Lazy.lazy_from_val l2)
let cons x l =
  app (Cons (x,Pqueue.empty)) l


let head = function
  | E -> raise Empty
  | Cons (a,_) -> a
let tail = function
  | E -> raise Empty
  | Cons (_,q) -> linkall q

let concat ls =
  List.fold_left app empty ls

let rec iter f l =
  if is_empty l then ()
  else f (head l); iter f (tail l)
