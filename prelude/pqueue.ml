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

