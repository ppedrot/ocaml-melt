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
let singleton x = Cons (x,Pqueue.empty)
let cons x l =
  app (singleton x) l


let head = function
  | E -> raise Empty
  | Cons (a,_) -> a
let tail = function
  | E -> raise Empty
  | Cons (_,q) -> linkall q

let rec fold_left f a l =
  if is_empty l then a
  else fold_left f (f a (head l)) (tail l)

let list_cons l t =
  List.fold_right cons l t

let of_list l =
  list_cons l empty

let list_concat ls =
  List.fold_left app empty ls

let concat ls =
  fold_left app empty ls

let rec iter f l =
  if is_empty l then ()
  else (f (head l); iter f (tail l))

let rec map f l =
  if is_empty l then empty
  else cons (f (head l)) (map f (tail l))

let rec forall p l =
  if is_empty l then true
  else p (head l) && forall p (tail l)
