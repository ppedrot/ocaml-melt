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



(**** Basic brick: atomic environments. ****)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module Environment : 
sig
  type t

  val empty : t
  val new_variable : ?eq:('a -> 'a -> bool) -> 'a -> (t -> 'a -> t) * (t -> 'a)

  val equal : t -> t -> bool

end =
struct

  type t = (bool -> bool) IntMap.t
    (* Those functions [f] hide a value in their closure.
       It can be accessed thanks to their side effet.
       [f true] is true iff the last function called, having the same
       identifier, holded the same value as [f].
       [f false] is true iff f hold the default value. *)

  let empty = IntMap.empty

  let new_id =
    let id = ref 0 in
    fun () -> incr id; !id

  let new_variable ?(eq=(=)) default =
    let id = new_id () in
    (* reference shared by all variable with the same id *)
    let v = ref default in
    (* add in the map [t] a function holding data [x] *)
    let set t x =
      let f test_x_equal_default =
	let old = !v in
	v := x;
	if test_x_equal_default then eq x default else eq x old
      in
      IntMap.add id f t
    in
    (* recover the hidden value *)
    let get t =
      ( try ignore ((IntMap.find id t) false) with Not_found -> v := default );
      !v
    in
    set,get

  let equal t1 t2 =
    (* fold returns keys in ascending order so to_list reverse to
       descending order *)
    let to_list t = IntMap.fold (fun k x l -> (k,x)::l) t [] in
    let l1 = to_list t1 in
    let l2 = to_list t2 in
    let rec check l1 l2 =
      match l1,l2 with
	| [], [] -> true
	| (k1,f1)::q1,(k2,f2)::q2 when k1 = k2 ->
	  let _ = f1 false in
	  (* now the shared reference [v] holds the value of f1 *)
	  let eq = f2 false in
	  (* f2 checks equality against the shared reference *)
	  if eq then check q1 q2 else false
	| (k1,f1)::q1,(k2,f2)::q2 when k1 > k2 ->
	  (* keys are in descending order, so k1 > k2 means that l1
	     has a key that l2 doesn't have *)
	  let eq = f1 true in
	  if eq then check q1 ((k2,f2)::q2) else false
	| (_,f1)::q1,[] ->
	  let eq = f1 true in
	  if eq then check q1 [] else false
	| l1,l2 ->
	  check l2 l1
    in
    check l1 l2

end



(**** Positions ****)

type position = { pos_name : string; pos_id : int }
let position : ?name:string -> unit -> position =
  let id = ref 0 in
  fun ?name () -> incr id;
    let name =
      match name with
	| None -> "unnamed position " ^ (string_of_int !id)
	| Some n -> n
    in
    { pos_name = name ; pos_id = !id }




(**** Variables ****)

type 'a t =
    { set : Environment.t -> 'a -> Environment.t;
      get : Environment.t -> 'a;
      equal : 'a -> 'a -> bool;
      var_name : string;
      var_printer : 'a -> string }
let make ?(eq=(=)) ?(name="unnamed") ?(printer=fun _ -> "undefined printer" ) x =
  let set,get = Environment.new_variable ~eq x in
  { set = set; get = get; equal = eq; var_name = name; var_printer = printer }




(**** Composed environments ****)

type env = {
  current : Environment.t ;
  positions : Environment.t IntMap.t ;
  placed : IntSet.t ;
  changed : position list ;
  change_log : position list list ;
}
let empty = {
  current = Environment.empty ;
  positions = IntMap.empty ;
  placed = IntSet.empty ;
  changed = [] ;
  change_log = []
}

let at_pos env position =
  match position with
  | None -> env.current
  | Some p ->
    begin
      try IntMap.find p.pos_id env.positions
      with Not_found -> Environment.empty
    end

let get_in_env ?position v env =
  let lenv = at_pos env position in
  v.get lenv


exception Multiple_place of position
let assert_not_placed env position =
  if IntSet.mem position.pos_id env.placed then
    raise (Multiple_place position)
  else
    { env with placed = IntSet.add position.pos_id env.placed }

let place_env env position  =
  let env = assert_not_placed env position in
  let new_env = env.current in
  if Environment.equal (at_pos env (Some position)) new_env then
    env
  else
    { env with
      positions = IntMap.add position.pos_id new_env env.positions;
      (* TODO récupérer les variables qui ont changé *)
      changed = position::env.changed }


(**** Extending documents ****)

type ('a,'b) with_var =
  | Raw of 'a
  | Set of (Environment.t -> Environment.t)
  (* update the environment in the remaining ast. *)
  | Get of position option * (Environment.t -> 'b)
  (* Looks for the environment at the given position (current
     location if it is None) and applies the get function
     to it. *)
  | Place of position
  (* Takes a snapshot of current environment an stores it
     in the given position. *)

let raw x = Raw x
let setf var f = Set (fun env -> var.set env (f (var.get env)))
let setf2 var1 var2 f =
  Set (fun env -> var2.set env (f (var1.get env) (var2.get env)))
let set var x = setf var (fun _ -> x)
let get ?position var k =
  Get (position , (fun env -> k (var.get env)))
let place position = Place position



let content = function
  | Raw y -> Some y
  | _ -> None


(**** Evaluating documents ****)

let compute empty encapsulate recurse env = function
  | Raw x -> encapsulate env x
  | Set s -> { env with current = s env.current } , empty
  | Get (p,g) -> recurse env (g (at_pos env p))
  | Place p -> place_env env p , empty


let reset_env env =
  { env with
    placed = IntSet.empty;
    changed = []
  }
let log_changes env =
  { env with
    change_log = env.changed::env.change_log
  }

exception Fixpoint_divergent of position list list

let default_fixpoint_iterations = ref 10

let rec fixpoint
    ?(env=empty) ?(iterations=(!default_fixpoint_iterations)) f x =
  if iterations = 0 then
    raise (Fixpoint_divergent env.change_log)
  else
    let (new_env,_) as r = f (reset_env env) x in
    match new_env.changed with
    | [] -> r
    | _ ->
      let new_env =
	(* Changes are logged, for error reporting,
           and the starting current environment is reset. *)
	{ (log_changes new_env) with
	  current = env.current }
      in
      fixpoint ~env:new_env
	       ~iterations:(iterations-1)
	       f x

(* TODO : control set_default_fixpoint_iterations from
   a melt option. *)
let set_default_fixpoint_iterations n =
  default_fixpoint_iterations := n
