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

open Printf

(* Use this to report errors. *)
let error x = ksprintf failwith x

module Opt = struct
  let iter f = function
    | Some x -> f x
    | None -> ()
  let map f = function
    | Some x -> Some (f x)
    | None -> None
  let default d = function
    | Some x -> x
    | None -> d
  let cons ox l = match ox with
    | Some x -> x::l
    | None -> l
  let fold f a = function
    | Some x -> f a x
    | None -> a
  let is_none = function
    | None -> true
    | Some _ -> false
end

module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end)

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

type env = Environment.t
type position = { pos_name : string; pos_id : int }

type mode = M | T | A
type arg_kind = Bracket | Brace | NoBr
let bracket = Bracket
let brace = Brace
let nobr = NoBr

type elt =
  | Command of string * (mode * arg_kind * t) list * mode
  | Text of string
  | Environment of string * (mode * t) option * (mode * t) list *
      (mode * t) * mode
  | Mode of mode * t
  | Set of ( env -> env )
  (* update the environment in the remaining ast, replaced by [Concat
     []] in fixpoint *)
  | Get of ( env -> t )
  (* fixpoint apply the function to the current environment to obtain
     [t], then replace this node by [t] then
     traverse [t]. *)
  | Get_position of ( env -> t ) * position
  (* like Get, but the environment is taken at the given position *)
  | Place of position
  (* fixpoint store the current environment under the name [position]
     and replace this node by [Concat []] *)
and t = elt Clist.t

let concat l = Clist.list_concat l
let empty = Clist.empty

(******************************************************************************)
(*                                  Variables                                 *)
(******************************************************************************)

type 'a variable =
    { set : Environment.t -> 'a -> Environment.t;
      get : Environment.t -> 'a;
      equal : 'a -> 'a -> bool;
      var_name : string;
      var_printer : 'a -> string }

let variable ?(eq=(=)) ?(name="unnamed") ?(printer=fun _ -> "undefined printer" ) x =
  let set,get = Environment.new_variable ~eq x in
  { set = set; get = get; equal = eq; var_name = name; var_printer = printer }

let position : ?name:string -> unit -> position =
  let id = ref 0 in
  fun ?name () -> incr id;
    let name =
      match name with
	| None -> "unnamed position " ^ (string_of_int !id)
	| Some n -> n
    in
    { pos_name = name ; pos_id = !id }

let get_in_env v env = v.get env

exception Multiple_place of position
exception Fixpoint_divergent of position list list


let fixpoint ?(env=Environment.empty) ?(iterations=10) t =
  (* TODO: controler le nombre d'iterations par une option de melt
     et mettre une fonction 'set number of iterations' *)
  let placed_tbl = Hashtbl.create 0 in
  (* table of already placed positions *)
  let position_tbl = Hashtbl.create 0 in
  (* position -> env *)
  let get_position_env pos =
    try Hashtbl.find position_tbl pos.pos_id
    with
      | Not_found -> Environment.empty
  in
  let assert_not_placed i =
    if Hashtbl.mem placed_tbl i
    then raise ( Multiple_place i )
    else Hashtbl.add placed_tbl i ()
  in
  let positions_changed = ref [] in
  (* list of position that changed during the current/last application of iter*)
  let changelog = ref [] in
  (* list of list of position that changed during each iteration *)
  let rec iter_elt env = function (* go througt the ast updating the environment and
				 appliying Get functions *)
    | Command (s, l , mode) ->
      let l,env =
	let aux (l,env) (mode,kind,t) =
	  let (t,env) = iter env t in
	  ((mode,kind,t)::l,env)
	in
	List.fold_left aux ([],env) l
      in
      Clist.singleton (Command (s, List.rev l, mode)),env
    | Mode (mode, t) ->
      let t,env = iter env t in
      Clist.singleton (Mode (mode, t)),env
    | Environment (s , opt, l, (mode, t), mode') ->
      (* l'ordre d'évaluation est gratuit, je ne sais pas si c'est le
	 bon *)
      let opt,env =
	match opt with
	  | None -> opt,env
	  | Some (mode,t) ->
	    let t,env = iter env t in
	    Some (mode,t),env
      in
      let l,env =
	let aux (l,env) (mode,t) =
	  let (t,env) = iter env t in
	  ((mode,t)::l,env)
	in
	List.fold_left aux ([],env) l
      in
      let t,env = iter env t in
      Clist.singleton begin
	Environment (s, opt, List.rev l, (mode, t), mode' )
      end,
      env
    | Text s -> (Clist.singleton (Text s),env)
    | Set setter ->
      empty,(setter env)
    | Get maker ->
      iter env (maker env)
    | Get_position (maker,position) ->
      let t = maker (get_position_env position) in
      iter env t
    | Place position ->
      assert_not_placed position;
      ( if Environment.equal (get_position_env position) env
	then ()
	else ( Hashtbl.replace position_tbl position.pos_id env;
	       positions_changed := position :: !positions_changed ));
      (* TODO récupérer les variables qui ont changé *)
      empty,env
  and iter env x =
    Clist.fold_left begin fun (x,env) t ->
      let (t,env) = iter_elt env t in (Clist.app x t,env)
    end (Clist.empty,env) x
  in
  let rec apply n =
    if n = 0 then raise (Fixpoint_divergent !changelog)
    else
      let (t,env) = iter env t in
      match !positions_changed with
	| [] -> t,env
	| l -> ( positions_changed := [];
		changelog := l :: !changelog;
		Hashtbl.clear placed_tbl;
		apply (n-1) )
  in
  apply iterations

let setf var f =
  Clist.singleton (Set (fun env -> var.set env (f (var.get env))))

let setf2 var1 var2 f =
  Clist.singleton (
    Set (fun env -> var2.set env (f (var1.get env) (var2.get env)))
  )

let get ?position var f =
  Clist.singleton begin match position with
    | None -> Get (fun env -> f (var.get env))
    | Some p -> Get_position ((fun env -> f (var.get env)),p)
  end

let place position =
  Clist.singleton (Place position)

let set x v = setf x (fun _ -> v)
let incr_var x = setf x (fun x -> x + 1)
let decr_var x = setf x (fun x -> x - 1)

let text s = Clist.singleton (Text s)

let vari x = get x (fun v -> text (string_of_int v))
let varf x = get x (fun v -> text (string_of_float v))
let varb x = get x (fun v -> text (string_of_bool v))
let vars x = get x text
let vart x = get x (fun v -> v)

let finalpos = position ~name:"final position" ()

let final var f =
  get ~position:finalpos var f

let finali x = final x (fun v -> text (string_of_int v))
let finalf x = final x (fun v -> text (string_of_float v))
let finalb x = final x (fun v -> text (string_of_bool v))
let finals x = final x text
let finalt x = final x (fun v -> v)

(******************************************************************************)
(*                           Packages and Commands                            *)
(******************************************************************************)

module PackageSet = Set.Make(struct
  type u = t * t
  type t = u
  let compare = compare
end)

let package_collector =
  let printer _ =
    (* TODO: il faudrait que to_string soit déclaré avant ca pour
       faire un printer *)
    "still to be written"
  in
  variable ~eq:PackageSet.equal
    ~name:"package collector" ~printer PackageSet.empty

let require_package acc package =
  concat [
    setf package_collector (PackageSet.add package);
    acc;
  ]

let require_packages packages =
  List.fold_left require_package empty packages

let require_package_string acc (a, b) =
  require_package acc (text a, text b)

let unusual_command ?(packages = []) name args mode =
  let thecommand = Clist.singleton (Command (name, args, mode)) in
  List.fold_left require_package_string thecommand packages

let command ?(packages=[]) name ?opt args mode =
    let opt = Opt.map (fun (m,t) -> (m,Bracket,t)) opt in
    let args = List.map (fun (m,t) -> (m,Brace,t)) args in
    let args = Opt.cons opt args in
    unusual_command ~packages name args mode

let environment ?(packages = []) name ?opt ?(args = []) body mode =
  let theenvironment =
    Clist.singleton (Environment(name, opt, args, body, mode))
  in
  List.fold_left
    require_package_string
    theenvironment
    packages

let (^^) x y = Clist.app x y
let mode mode x = Clist.singleton (Mode(mode, x))

let latex = command "LaTeX" [] T

let usepackage ?opt name =
  let opt = Opt.map (fun x -> T, x) opt in
  command "usepackage" ?opt [T, name] T ^^ text"\n"

let final_usepackages =
  get ~position:finalpos package_collector
    (fun pc ->
      concat
        (PackageSet.fold
           (fun (name, opt) acc -> usepackage ~opt name :: acc)
           pc []))

(******************************************************************************)

module Pp: sig
  type t
  val make: Buffer.t -> t
  val string: t -> string -> unit
  val char: t -> char -> unit
  val newline: ?force: bool -> t -> unit
  val bol: t -> unit (* new line if not already at a new line *)
  val indent: t -> int -> unit (* relative indentation of the next line *)
  val space: t -> unit (* space if not already after a space *)
end = struct
  type t = {
    buf: Buffer.t;
    mutable bol: bool;
    mutable alinea: string;
    mutable indent: int;
    mutable last_char: char;
    mutable line_empty: bool;
  }

  let output_char x c =
    Buffer.add_char x.buf c;
    x.last_char <- c;
    if c <> ' ' then x.line_empty <- false

  let newline ?(force = false) x =
    if force || not x.line_empty then begin
      output_char x '\n';
      x.bol <- true;
      x.line_empty <- true
    end

  let bol x =
    if not x.bol then newline x

  let rec string_split acc from c s =
    let index = try String.index_from s from c with Not_found -> -1 in
    if index = -1 then
      let sub = String.sub s from (String.length s - from) in
      List.rev (sub::acc)
    else
      let sub = String.sub s from (index-from) in
      string_split (sub::acc) (index+1) c s
  let string_split x = string_split [] 0 x

  let output_string x s =
    if String.length s <> 0 then begin
      Buffer.add_string x.buf s;
      x.last_char <- s.[String.length s - 1]
    end;
    let empty = ref true in
    begin try
      for i = 0 to String.length s - 1 do
        if s.[i] <> ' ' then begin
          empty := false;
          raise Not_found
        end
      done
    with Not_found -> () end;
    x.line_empty <- x.line_empty && !empty

  let output_string x s =
    match string_split '\n' s with
      | [] -> ()
      | [s] -> output_string x s
      | s::rem ->
          output_string x s;
          List.iter (fun s -> bol x; output_string x s) rem

  let make buf = {
    buf = buf;
    bol = true;
    alinea = "";
    indent = 0;
    last_char = ' ';
    line_empty = true;
  }

  let force_alinea x =
    if x.bol then output_string x x.alinea;
    x.bol <- false

  let string x s =
    force_alinea x;
    output_string x s

  let char x c =
    force_alinea x;
    output_char x c

  let indent x i =
    x.indent <- x.indent + i;
    x.alinea <- if x.indent <= 0 then "" else String.make x.indent ' '

  let space x =
    if x.last_char <> ' ' then begin
      output_char x ' ';
      x.last_char <- ' '
    end
end

let ensure_mode pp from_mode to_mode f = match from_mode, to_mode with
  | M, M
  | T, T
  | A, _
  | _, A -> f ()
  | M, T -> Pp.char pp '$'; f (); Pp.char pp '$'
  | T, M -> Pp.string pp "\\mbox{"; f (); Pp.char pp '}'

(* bol: "beginning of line" *)
let rec out_elt toplevel mode pp = function
  | Text "\\par " when toplevel ->
      Pp.bol pp;
      Pp.newline ~force: true pp
  | Command(name, args, rm) ->
      ensure_mode pp rm mode begin fun () ->
        if args = [] && mode = T then Pp.char pp '{';
        Pp.char pp '\\';
        Pp.string pp name;
        if args = [] && mode = M then Pp.space pp;
        if args = [] && mode = T then Pp.char pp '}';
        out_args pp args
      end
  | Environment(name, opt, args, (bodymode, body), rm) ->
      ensure_mode pp rm mode begin fun () ->
        Pp.bol pp;
        Pp.string pp "\\begin{";
        Pp.string pp name;
        Pp.char pp '}';
        Opt.iter (command_argument_brackets pp) opt;
        List.iter (command_argument_braces pp) args;
        Pp.indent pp 2;
        Pp.bol pp;
        out true bodymode pp body;
        Pp.indent pp (-2);
        Pp.bol pp;
        Pp.string pp "\\end{";
        Pp.string pp name;
        Pp.char pp '}';
        Pp.bol pp
      end
  | Text s ->
      Pp.string pp s
  | Mode(m, x) ->
      ensure_mode pp m mode (fun () -> out false m pp x)
  | Set _
  | Get _
  | Get_position _
  | Place _ -> assert false
and out toplevel mode pp x =
  Clist.iter (out_elt toplevel mode pp) x

and command_argument pp (mode, x) before after =
  Pp.string pp before;
  out false mode pp x;
  Pp.string pp after

and command_argument_braces pp ca = command_argument pp ca "{" "}"
and command_argument_brackets pp ca = command_argument pp ca "[" "]"
and command_argument_nobr pp ca = command_argument pp ca " " ""

and out_args =
  let out_arg pp = function
    | (m,Bracket,t) -> command_argument_brackets pp (m,t)
    | (m,Brace,t) -> command_argument_braces pp (m,t)
    | (m,NoBr,t) -> command_argument_nobr pp (m,t)
  in
  fun pp args ->
  List.iter (out_arg pp) args

let to_buffer ?(mode = T) ?env buf x =
  let x,env = (fixpoint ?env (concat [x;place finalpos])) in
  out true mode (Pp.make buf) x;
  env

let to_channel ?mode ?env c x =
  let buf = Buffer.create 69 in
  let env = to_buffer ?env ?mode buf x in
  Buffer.output_buffer c buf;
  env

let to_file ?mode ?env f x =
  let oc = open_out f in
  let env = to_channel ?env ?mode oc x in
  close_out oc;
  env

let to_string_with_env ?mode ?env x =
  let buf = Buffer.create 69 in
  let env = to_buffer ?env ?mode buf x in
  Buffer.contents buf, env

let to_string ?mode x =
  fst (to_string_with_env ?mode x)

(*******************************************************************************)

let list_insert sep = function
  | [] | [_] as x -> x
  | x::rem -> List.flatten ([x]::(List.map (fun x -> [sep; x]) rem))

let rec list_filter_options acc = function
  | [] -> List.rev acc
  | None :: rem -> list_filter_options acc rem
  | Some x :: rem -> list_filter_options (x :: acc) rem
let list_filter_options = list_filter_options []

let make_option ?(sep = ",") mode f o =
  if o = [] then None else
    let o = list_insert (text sep) (List.map (fun x -> text x) (List.map f o)) in
    Some(mode, concat o)

type size = [
| `In of float
| `Mm of float
| `Cm of float
| `Pt of float
| `Em of float
| `Ex of float

| `Pc of float
| `Bp of float
| `Dd of float
| `Cc of float
| `Sp of float

| `Parindent of float
| `Baselineskip of float
| `Baselinestretch of float
| `Parskip of float
| `Textwidth of float
| `Linewidth of float
| `Textheight of float
| `Unitlength of float

| `Fill
| `Stretch of int
]

let string_of_size size =
  match size with
    | `In x -> sprintf "%fin" x
    | `Mm x -> sprintf "%fmm" x
    | `Cm x -> sprintf "%fcm" x
    | `Pt x -> sprintf "%fpt" x
    | `Em x -> sprintf "%fem" x
    | `Ex x -> sprintf "%fex" x

    | `Pc x -> sprintf "%fpc" x
    | `Bp x -> sprintf "%fbp" x
    | `Dd x -> sprintf "%fdd" x
    | `Cc x -> sprintf "%fcc" x
    | `Sp x -> sprintf "%fsp" x

    | `Parindent x -> sprintf "%f\\parindent" x
    | `Baselineskip x -> sprintf "%f\\baselineskip" x
    | `Baselinestretch x -> sprintf "%f\\baselinestretch" x
    | `Parskip x -> sprintf "%f\\parskip" x
    | `Textwidth x -> sprintf "%f\\textwidth" x
    | `Linewidth x -> sprintf "%f\\linewidth" x
    | `Textheight x -> sprintf "%f\\textheight" x
    | `Unitlength x -> sprintf "%f\\unitlength" x

    | `Fill -> sprintf "\\fill"
    | `Stretch x -> sprintf "\\stretch{%i}" x

let latex_of_size size = text (string_of_size size)

let latex_of_int x = text (string_of_int x)
let latex_of_float x = text (string_of_float x)

let rec is_empty_elt = function
  | Text "" -> true
  | Mode(_, y) -> is_empty y
  | Command _ | Environment _ | Text _
  | Set _ | Get _ | Get_position _ | Place _ ->
      (* You would assume that these should disappear after [compute_variables]
         is called. However, when building the AST, this is not actually
         the case, yet. *)
      false
and is_empty x =
  Clist.forall is_empty_elt x

let none_if_empty x =
  if is_empty x then None else Some x

let optcmd name = function
  | Some arg -> command name [T, arg] T
  | None -> empty

let labelo l = optcmd "label" (Opt.map text l)

let par = text "\\par "

(*******************************************************************************)

type label = string

let label =
  let cnt = ref 0 in
  fun ?name () ->
    match name with
      | None ->
          incr cnt;
          "latex_lib_label_" ^ string_of_int !cnt
      | Some name -> name

let ref_ l = command "ref" [T, text l] T

let place_label l = command "label" [T, text l] T

(******************************************************************************)

(* Index construction *)

let needs_indexing = variable ~name:"needs indexing" ~printer:string_of_bool false
let place_index key =
  concat
    [set needs_indexing true;
     (command "index" ~packages:["makeidx",""] [T,key] T)]
let printindex =
  concat
    [set needs_indexing true;
     (command "printindex" ~packages:["makeidx",""] [] T)]
let start_indexing =
  get ~position:finalpos needs_indexing
    begin function
      | true -> text "\\makeindex"
      | false -> empty
    end

(******************************************************************************)

type documentclass = 
    [ `Article | `Report | `Book | `Letter | `Slides | `Beamer
    | `Custom of string ]
type documentoptions = [ `Landscape | `A4paper | `TwoColumn | `Pt of
    int ]

let input file = command "input" [T,file] T

let newcommand count name body = 
  command "newcommand"
    ?opt: (if count = 0 then None else Some(T, latex_of_int count))
    [T, name; T, body] T

let renewcommand count name body = 
  command "renewcommand"
    ?opt: (if count = 0 then None else Some(T, latex_of_int count))
    [T, name; T, body] T

let required_packages = concat [
  final_usepackages ;
  par;
  start_indexing; 
]

let documentclass ?opt cl = command "documentclass" ?opt [T,cl] T
let documentmatter x = environment "document" (T, x) T

(* to be able to use a [documentclass] label *)
let _documentclass = documentclass

let document ?(documentclass=`Article) ?(options=[]) ?title ?author
    ?date ?(prelude=empty) ?(packages=[]) body =
  let dc = match documentclass with
    | `Article -> "article"
    | `Report -> "report"
    | `Book -> "book"
    | `Letter -> "letter"
    | `Slides -> "slides"
    | `Beamer -> "beamer"
    | `Custom c -> c
  in
  let options = make_option T begin function
    | `Landscape -> "landscape"
    | `A4paper -> "a4paper"
    | `TwoColumn -> "twocolumn"
    | `Pt i -> (string_of_int i)^"pt"
  end options in
  let body = if title <> None then command "maketitle" [] T ^^ body else body in
  concat [
    _documentclass ?opt:options (text dc);
    par;
    require_packages packages;
    required_packages; par;
    prelude;
    par;
    optcmd "title" title; text"\n";
    optcmd "author" author; text"\n";
    optcmd "date" date; text"\n";
    par;
    documentmatter body;
  ]

let within_braces x = text "{" ^^ x ^^ text "}"
let block x = match none_if_empty x with
  | None -> empty
  | Some x -> within_braces x
let index x y = mode M (block x ^^ text "_" ^^ block y)
let exponent x y = mode M (block x ^^ text "^" ^^ block y)
let index_exponent x y z =
  mode M (block x ^^ text "_" ^^ block y ^^ text "^" ^^ block z)

let part ?label title =
  command "part" [T, title] T ^^ labelo label

let chapter ?label title =
  command "chapter" [T, title] T ^^ labelo label

let section ?label title =
  command "section" [T, title] T ^^ labelo label

let subsection ?label title =
  command "subsection" [T, title] T ^^ labelo label

let subsubsection ?label title =
  command "subsubsection" [T, title] T ^^ labelo label

let paragraph title =
  command "paragraph" [T, title] T

let displaymath x =
  environment "displaymath" (M, x) T

let equation ?label x =
  environment "equation" (M, labelo label ^^ x) T

let emph x = command "emph" [T, x] T

let today = command "today" [] T
let maketitle = command "maketitle" [] T

let texttt x = command "texttt" [T, x] T
let textsc x = command "textsc" [T, x] T
let textit x = command "textit" [T, x] T
let textbf x = command "textbf" [T, x] T
let textrm x = command "textrm" [T, x] T
let textsf x = command "textsf" [T, x] T

let mathit x = command "mathit" [M, x] M
let mathbf x = command "mathbf" [M, x] M
let mathcal x = command "mathcal" [M, x] M
let mathsf t = command "mathsf" [M,t] M

let tiny x = block (command "tiny" [T, x] T)
let scriptsize x = block (command "scriptsize" [T, x] T)
let footnotesize x = block (command "footnotesize" [T, x] T)
let small x = block (command "small" [T, x] T)
let normalsize x = block (command "normalsize" [T, x] T)
let large x = block (command "large" [T, x] T)
let large2 x = block (command "Large" [T, x] T)
let large3 x = block (command "LARGE" [T, x] T)
let huge x = block (command "huge" [T, x] T)
let huge2 x = block (command "Huge" [T, x] T)

let hfill = command "hfill" [] T

let footnote x = command "footnote" [T, x] T

let tableofcontents = command "tableofcontents" [] T
let listoffigures = command "listoffigures" [] T
let listoftables = command "listoftables" [] T

type alignment = [ `L | `C | `R ]
type array_column =  [ alignment | `Vert | `Sep of t]

type array_line_normal = {
  al_columns: (int*[alignment | `I]*t) list;
  al_sep: size option;
}

type array_line =
  | ArrayNormal of array_line_normal
  | ArrayCommand of t

let array_line ?sep ?layout xs = 
  let xs =
    match layout with
    | None -> List.map (fun x -> (1,`I,x)) xs
    | Some l -> 
	try List.map2 (fun x (i,a) -> (i,a,x)) xs l
	with Invalid_argument _ ->
	  error "array_line: %d columns but layout only supports %d."
            (List.length xs) (List.length l)
  in
  ArrayNormal { al_columns = xs;
    al_sep = sep 
  }

let array_command x = ArrayCommand x

let array_line_width al =
  List.fold_left (fun acc (w,_,_) -> acc+w) 0 al.al_columns
let array_line_mapi f al =
  let rec array_line_mapi al i =
    match al with
    | [] -> []
    | (w,_,_) as x :: l -> (f i x)::(array_line_mapi l (i+w))
  in
  array_line_mapi al.al_columns 0

let newline = text "\\\\\n"
let newline_size x = text (Printf.sprintf "\\\\[%s]\n" (string_of_size x))

let newpage = command "newpage" [] T
let clearpage = command "clearpage" [] T

let newlinegen = function
  | None -> newline
  | Some x -> newline_size x

let space = command " " [] A
let quad = command "quad" [] M
let qquad = command "qquad" [] M

let includegraphics filename = command ~packages: ["graphicx", ""]
  "includegraphics" [ A, filename ] T

let symbol i = command "symbol" [T, latex_of_int i] T
let symbolc c = symbol (Char.code c)

type float_position = [ `H | `T | `P | `B | `Force ]

let float_all = [ `H; `T; `B; `P ]

let generic_figure_contents ?label ?(center = false) ?caption body =
  let body = if center then text "\\centering{}" ^^ body else body in
  let body = match caption with
    | None -> body
    | Some caption -> body ^^ command "caption" [T, caption] T
  in
  let body = body ^^ labelo label in
  body

let figure ?label ?(pos = [ `H ]) ?center ?(side_caption = false) ?caption
    ?(wide = false) body =
  let spos = String.concat "" begin List.map begin function
    | `H -> "h"
    | `T -> "t"
    | `P -> "p"
    | `B -> "b"
    | `Force -> "!"
  end pos end in
  let body = generic_figure_contents ?label ?center ?caption body in
  let name = match side_caption, wide with
    | false, false -> "figure"
    | true, false -> "SCfigure"
    | false, true -> "figure*"
    | true, true -> "SCfigure*"
  in
  let packages = if side_caption then [ "sidecap", "" ] else [] in
  let packages =
    if wide && List.mem `B pos then
      ("stfloats", "") :: packages
    else
      packages
  in
  environment ~packages ~opt: (A, text spos) name (T, body) T

type wrapfigure_position =
    [ `L | `R | `I | `O | `Force of [ `L | `R | `I | `O ] ]

let wrapfigure ?label ?(pos: wrapfigure_position = `R)
    ?lines ?(width: size = `Textwidth 0.5) ?center ?caption body =
  let pos = match pos with
    | `L -> "L"
    | `R -> "R"
    | `I -> "I"
    | `O -> "O"
    | `Force `L -> "l"
    | `Force `R -> "r"
    | `Force `I -> "i"
    | `Force `O -> "o"
  in
  let body = generic_figure_contents ?label ?center ?caption body in
  let opt = Opt.map (fun i -> A, latex_of_int i) lines in
  environment
    ?opt
    ~packages: [ "wrapfig", "" ]
    ~args: [A, text pos; A, latex_of_size width]
    "wrapfigure" (T, body) T

type floatingfigure_position = [ `L | `R | `P ]

let floatingfigure ?label ?(pos: floatingfigure_position = `R)
    ?(width: size = `Textwidth 0.5) ?center ?caption body =
  let pos = match pos with
    | `R -> "r"
    | `L -> "l"
    | `P -> "p"
  in
  let body = generic_figure_contents ?label ?center ?caption body in
  environment
    ~opt: (A, text pos)
    ~packages: [ "floatflt", "" ]
    ~args: [A, latex_of_size width]
    "floatingfigure" (T, body) T

let subfloat ?label ?caption body =
  let opt = Opt.map (fun c -> T, c) caption in
  let body = labelo label ^^ body in
  environment ?opt ~packages: [ "subfig", "" ] "subfloat" (T, body) T

let minipage size x =
  environment ~args: [A, latex_of_size size] "minipage" (T, x) T

let center x = environment "center" (T, x) T
let flushleft x = environment "flushleft" (T,x) T
let flushright x = environment "flushright" (T,x) T

let latex_of_array_column = function
  | `L -> text "l"
  | `C -> text "c"
  | `R -> text "r"
  | `Vert -> text "|"
  | `Sep t -> concat [ text "@{" ; t ; text "}"] 
let multicolumn w a x =
  command "multicolumn" [(A,latex_of_int w); (A,latex_of_array_column a); (M,x)] A
let array c l =
  let cols = concat begin List.map latex_of_array_column c end in
  let alignments = 
    Array.of_list (List.filter (function #alignment -> true | _ -> false) c)
  in
  let numcols = Array.length alignments in
  let multicolumn i (w,a,x)=
    match a with
    | `I -> 
	if w = 1 then (*spiwack: do we need to take care of cases <1? *)
	  x
	else
	  multicolumn w (alignments.(i)) x
    | #array_column as a -> multicolumn w a x
  in
  let lines = List.map begin fun al ->
    match al with
    | ArrayNormal al ->
	begin
	  let width = array_line_width al in
	  if width <> numcols then
	    error "array: line with %d columns instead of %d" width numcols;
	  let lc = array_line_mapi multicolumn al in
	  concat (list_insert (text " & ") lc) ^^ newlinegen al.al_sep
	end
    | ArrayCommand x -> x ^^ text"\n"
  end l in
  let body = concat lines (*(list_insert newline lines)*) in
  environment "array" ~args: [M, cols] (M, body) M

let list_env l name = 
(*  let items = List.map ((^^) (command "item" [] T)) l in*)
  (* Latex produces an error with empty itemize or enumerate. We might as
     well produce the error ourself. *)
  if l = [] then error "itemize or enumerate: no item given";
  let items = List.map ((^^) (text "\\item ")) l in
  let body = concat (list_insert (text "\n") items) in
  environment name (T, body) T

let itemize l = list_env l "itemize"
let enumerate l = list_env l "enumerate"

let vspace s = command "vspace" [T, latex_of_size s] T
let hspace s = command "hspace" [T, latex_of_size s] T
let addvspace s = command "addvspace" [T, latex_of_size s] T
let smallskip = command "smallskip" [] T
let medskip = command "medskip" [] T
let bigskip = command "bigskip" [] T
let nointerlineskip = command "nointerlineskip" [] T

let phantom x = command "phantom" [T, x] T
let vphantom x = command "vphantom" [T, x] T
let hphantom x = command "hphantom" [T, x] T

let parbox x y = command "parbox" [A, latex_of_size x; T, y] T

type halignment = [ `C | `L | `R | `S ]
let latex_of_halignment = function
  | `C -> text "c" 
  | `L -> text "l"
  | `R -> text "r"
  | `S -> text "s"
let makeframebox name size ?halign t =
  let size = (A, bracket,latex_of_size size) in
  let halign = Opt.map (fun h -> (A,bracket,latex_of_halignment h)) halign in
  let t = (T,brace,t) in
  unusual_command name (size::(Opt.cons halign [t])) T
let makebox = makeframebox "makebox"
let framebox = makeframebox "framebox"

let noindent = command "noindent" [] T

let stackrel x y = command "stackrel" [M, x; M, y] M

(*******************************************************************************)

let box_ = command ~packages: [ "latexsym", "" ] "Box" [] M

let langle = command "langle" [] M
let rangle = command "rangle" [] M
let lceil = command "lceil" [] M
let rceil = command "rceil" [] M

let frac x y = command "frac" [M, x; M, y] M

let land_ = command "land" [] M
let lor_ = command "lor" [] M
let lnot = command "lnot" [] M
let forall = command "forall" [] M
let exists = command "exists" [] M

let top = command "top" [] M
let bot = command "bot" [] M
let cdots = command "cdots" [] M

let sharp  =command "sharp" [] M

let emptyset = command "emptyset" [] M

type doublable_delimiter =
    [ `Down | `Up | `Up_down | `Vert ]
type delimiter =
    [ `None | `Brace | `Paren | `Bracket | `Angle | `Floor | `Ceil | `Slash
    | doublable_delimiter | `Double of doublable_delimiter ]

let ambidexter_delimiter_to_text = function
  | `Down            -> "\\downarrow"
  | `Up              -> "\\uparrow"
  | `Up_down         -> "\\updownarrow"
  | `Vert            -> "|"
  | `Double `Down    -> "\\Downarrow"
  | `Double `Up      -> "\\Uparrow"
  | `Double `Up_down -> "\\Updownarrow"
  | `Double `Vert    -> "\\|"

let delimiter_to_left_text: delimiter -> string = function
  | `None    -> "."
  | `Brace   -> "\\{"
  | `Paren   -> "("
  | `Bracket -> "["
  | `Angle   -> "\\langle"
  | `Floor   -> "\\lfloor"
  | `Ceil    -> "\\lceil"
  | `Slash   -> "/"
  | `Down | `Up | `Up_down | `Vert | `Double _ as x ->
      ambidexter_delimiter_to_text x

let delimiter_to_right_text = function
  | `None    -> "."
  | `Brace   -> "\\}"
  | `Paren   -> ")"
  | `Bracket -> "]"
  | `Angle   -> "\\rangle"
  | `Floor   -> "\\rfloor"
  | `Ceil    -> "\\rceil"
  | `Slash   -> "\\backslash"
  | `Down | `Up | `Up_down | `Vert | `Double _ as x ->
      ambidexter_delimiter_to_text x

let left d = command (sprintf "left%s" (delimiter_to_left_text d)) [] M
let right d = command (sprintf "right%s" (delimiter_to_right_text d)) [] M
let just_left d x = mode M (concat [left d; x; right `None])
let just_right d x = mode M (concat [left `None; x; right d])
let between d x = mode M (concat [left d; x; right d])

let oe = command "oe" [] T

(*******************************************************************************)

let hat x = command "hat" [M, x] M
let grave x = command "grave" [M, x] M
let bar x = command "bar" [M, x] M
let acute x = command "acute" [M, x] M
let mathring x = command "mathring" [M, x] M
let check x = command "check" [M, x] M
let dot x = command "dot" [M, x] M
let vec x = command "vec" [M, x] M
let breve x = command "breve" [M, x] M
let tilde x = command "tilde" [M, x] M
let ddot x = command "ddot" [M, x] M
let widehat x = command "widehat" [M, x] M
let widetilde x = command "widetilde" [M, x] M

(*******************************************************************************)

let alpha = command "alpha" [] M
let beta = command "beta" [] M
let gamma = command "gamma" [] M
let delta = command "delta" [] M
let epsilon = command "epsilon" [] M
let varepsilon = command "varepsilon" [] M
let zeta = command "zeta" [] M
let eta = command "eta" [] M
let theta = command "theta" [] M
let vartheta = command "vartheta" [] M
let iota = command "iota" [] M
let kappa = command "kappa" [] M
let varkappa = command "varkappa" ~packages:["amssymb",""] [] M
let lambda = command "lambda" [] M
let mu = command "mu" [] M
let nu = command "nu" [] M
let xi = command "xi" [] M
let pi = command "pi" [] M
let varpi = command "varpi" [] M
let rho = command "rho" [] M
let varrho = command "varrho" [] M
let sigma = command "sigma" [] M
let varsigma = command "varsigma" [] M
let tau = command "tau" [] M
let upsilon = command "upsilon" [] M
let phi = command "phi" [] M
let varphi = command "varphi" [] M
let chi = command "chi" [] M
let psi = command "psi" [] M
let omega = command "omega" [] M

let digamma = command "digamma" ~packages:["amssymb",""] [] M

let gamma_ = command "Gamma" [] M
let delta_ = command "Delta" [] M
let theta_ = command "Theta" [] M
let lambda_ = command "Lambda" [] M
let xi_ = command "Xi" [] M
let pi_ = command "Pi" [] M
let sigma_ = command "Sigma" [] M
let upsilon_ = command "Upsilon" [] M
let phi_ = command "Phi" [] M
let psi_ = command "Psi" [] M
let omega_ = command "Omega" [] M

let aleph = command "aleph" [] M
let beth = command "beth" ~packages:["amssymb",""] [] M
let gimel = command "gimel" ~packages:["amssymb",""] [] M
let daleth = command "daleth" ~packages:["amssymb",""] [] M

(*******************************************************************************)

let le = command "le" [] M
let ge = command "ge" [] M
let leqslant = command "leqslant" ~packages:["amssymb",""] [] M
let geqslant = command "geqslant" ~packages:["amssymb",""] [] M
let equiv = command "equiv" [] M
let ll = command "ll" [] M
let gg = command "gg" [] M
let doteq = command "doteq" [] M
let prec = command "prec" [] M
let succ = command "succ" [] M
let sim = command "sim" [] M
let preceq = command "preceq" [] M
let succeq = command "succeq" [] M
let simeq = command "simeq" [] M
let subset = command "subset" [] M
let supset = command "supset" [] M
let approx = command "approx" [] M
let subseteq = command "subseteq" [] M
let supseteq = command "supseteq" [] M
let cong = command "cong" [] M
let sqsubset = command "sqsubset" [] M
let sqsupset = command "sqsupset" [] M
let join_ = command "Join" [] M
let sqsubseteq = command "sqsubseteq" [] M
let sqsupseteq = command "sqsupseteq" [] M
let bowtie = command "bowtie" [] M
let in_ = command "in" [] M
let owns = command "owns" [] M
let propto = command "propto" [] M
let vdash = command "vdash" [] M
let dashv = command "dashv" [] M
let models = command "models" [] M
let mid = command "mid" [] M
let parallel = command "parallel" [] M
let perp = command "perp" [] M
let smile = command "smile" [] M
let frown = command "frown" [] M
let asymp = command "asymp" [] M
let notin = command "notin" [] M
let ne = command "ne" [] M

(*******************************************************************************)

let pm = command "pm" [] M
let mp = command "mp" [] M
let triangleleft = command "triangleleft" [] M
let cdot = command "cdot" [] M
let div = command "div" [] M
let triangleright = command "triangleright" [] M
let times = command "times" [] M
let setminus = command "setminus" [] M
let star = command "star" [] M
let cup = command "cup" [] M
let cap = command "cap" [] M
let ast = command "ast" [] M
let sqcup = command "sqcup" [] M
let sqcap = command "sqcap" [] M
let circ = command "circ" [] M
let lor_ = command "lor" [] M
let land_ = command "land" [] M
let bullet = command "bullet" [] M
let oplus = command "oplus" [] M
let ominus = command "ominus" [] M
let diamond = command "diamond" [] M
let odot = command "odot" [] M
let oslash = command "oslash" [] M
let uplus = command "uplus" [] M
let otimes = command "otimes" [] M
let bigcirc = command "bigcirc" [] M
let amalg = command "amalg" [] M
let bigtriangleup = command "bigtriangleup" [] M
let bigtriangledown = command "bigtriangledown" [] M
let dagger = command "dagger" [] M
let lhd = command ~packages: ["latexsym", ""] "lhd" [] M
let rhd = command ~packages: ["latexsym", ""] "rhd" [] M
let ddagger = command "ddagger" [] M
let unlhd = command "unlhd" [] M
let unrhd = command "unrhd" [] M
let wr = command "wr" [] M

(*******************************************************************************)

let sum = command "sum" [] M
let prod = command "prod" [] M
let coprod = command "coprod" [] M
let bigcup = command "bigcup" [] M
let bigcap = command "bigcap" [] M
let bigvee = command "bigvee" [] M
let bigwedge = command "bigwedge" [] M
let bigsqcup = command "bigsqcup" [] M
let biguplus = command "biguplus" [] M
let int = command "int" [] M
let oint = command "oint" [] M
let bigodot = command "bigodot" [] M
let bigoplus = command "bigoplus" [] M
let bigotimes = command "bigotimes" [] M

(*******************************************************************************)

let leftarrow = command "leftarrow" [] M
let rightarrow = command "rightarrow" [] M
let leftrightarrow = command "leftrightarrow" [] M
let leftarrow_ = command "Leftarrow" [] M
let rightarrow_ = command "Rightarrow" [] M
let leftrightarrow_ = command "Leftrightarrow" [] M
let longleftarrow = command "longleftarrow" [] M
let longrightarrow = command "longrightarrow" [] M
let longleftrightarrow = command "longleftrightarrow" [] M
let longleftarrow_ = command "Longleftarrow" [] M
let longrightarrow_ = command "Longrightarrow" [] M
let longleftrightarrow_ = command "Longleftrightarrow" [] M
let iff = command "iff" [] M

let mapsto = command "mapsto" [] M
let longmapsto = command "longmapsto" [] M
let hookleftarrow = command "hookleftarrow" [] M
let hookrightarrow = command "hookrightarrow" [] M
let leftharpoonup = command "leftharpoonup" [] M
let rightharpoonup = command "rightharpoonup" [] M
let leftharpoondown = command "leftharpoondown" [] M
let rightharpoondown = command "rightharpoondown" [] M
let rightleftharpoons = command "rightleftharpoons" [] M
let uparrow = command "uparrow" [] M
let downarrow = command "downarrow" [] M
let updownarrow = command "updownarrow" [] M
let uparrow_ = command "Uparrow" [] M
let downarrow_ = command "Downarrow" [] M
let updownarrow_ = command "Updownarrow" [] M
let nearrow = command "nearrow" [] M
let searrow = command "searrow" [] M
let swarrow = command "swarrow" [] M
let nwarrow = command "nwarrow" [] M
let leadsto = command ~packages: ["latexsym", ""] "leadsto" [] M

(*******************************************************************************)

let mathbb x = command "mathbb" ~packages:["amssymb",""] [M, x] M
let align x = environment "align" (M, x) T
let align_ x = environment "align*" (M, x) T
let gather x = environment "gather" (M, x) T
let gather_ x = environment "gather*" (M, x) T
let split x = environment "split" (M,x) M

let proof ?opt t = 
  let opt = Opt.map (fun x -> T, x) opt in
  environment "proof" ?opt (T, t) T

let twoheadrightarrow = command "twoheadrightarrow" [] M
let square = command ~packages:["amssymb",""] "square" [] M

(*******************************************************************************)

let mathpar l = 
  let content =
    M,
    concat (list_insert (command ~packages: ["mathpartir", ""] "and" [] M) l) in
  environment ~packages: ["mathpartir", ""] "mathpar" content  T

let inferrule ?lab ?left ?right ?vdots ?width ?leftskip ?rightskip lx ly = 
  let lx, ly = match lx, ly with
    | [], [] -> [text "~"], []
    | _, _ -> lx, ly
  in
  let cx = concat (list_insert newline lx) in
  let cy = concat (list_insert newline ly) in
  let left = Opt.map (fun x -> text "left=" ^^ x) left in
  let right = Opt.map (fun x -> text "right=" ^^ x) right in
  let lab = Opt.map (fun x -> text "lab=" ^^ x) lab in
  let vdots = Opt.map (fun x -> text "vdots=" ^^ latex_of_size x) vdots in
  let width = Opt.map (fun x -> text "width=" ^^ latex_of_size x) width in
  let leftskip =
    Opt.map (fun x -> text "leftskip=" ^^ latex_of_size x) leftskip in
  let rightskip =
    Opt.map (fun x -> text "rightskip=" ^^ latex_of_size x) rightskip in
  let option_list = [ left; right; lab; vdots; width; leftskip; rightskip ] in
  let opt =
    concat (list_insert (text ",") (list_filter_options option_list)) in
  command ~packages: ["mathpartir", ""] ~opt: (A, opt)
    "inferrule*" [M, cx; M, cy] M

(*******************************************************************************)

let cmd_stmaryrd = command ~packages: ["stmaryrd", ""]

let llbracket = cmd_stmaryrd "llbracket" [] M
let rrbracket = cmd_stmaryrd "rrbracket" [] M

(*******************************************************************************)

let slide x =
  environment "slide" (T, x) T

(*******************************************************************************)

(* Some contributions from Vincent Aravantinos *)

let cmd_no_arg ?(packages=[]) cmd =
  command ~packages cmd [] T
let cmd_one_arg ?(packages=[]) cmd arg =
  command ~packages cmd [T,arg] T
let cmd_two_args ?(packages=[]) cmd arg1 arg2 =
  command ~packages cmd [T,arg1;T,arg2] T
let math_cmd_no_arg ?(packages=[]) cmd =
  command ~packages cmd [] M
let math_cmd_one_arg ?(packages=[]) cmd arg =
  command ~packages cmd [M,arg] M
let math_cmd_two_args ?(packages=[]) cmd arg1 arg2 =
  command ~packages cmd [M,arg1;M,arg2] M

(* Misc *)
let par_ = cmd_no_arg "S"
let hyphen = cmd_no_arg "-"
let quote txt = environment "quote" (T,txt) T
let quotation txt = environment "quotation" (T,txt) T
let appendix = cmd_no_arg "appendix"
let neg = math_cmd_no_arg "neg"
let mathrm = math_cmd_one_arg "mathrm"
let mathfrak = math_cmd_one_arg "mathfrak"
let frontmatter = cmd_no_arg "frontmatter"
let backmatter = cmd_no_arg "backmatter"
let mainmatter = cmd_no_arg "mainmatter"
let geq = math_cmd_no_arg "geq"
let leq = math_cmd_no_arg "leq"
let dots = cmd_no_arg "dots"
let ldots = cmd_no_arg "ldots"
let underbrace x y = (math_cmd_no_arg "underbrace")^^(index x y)
let overbrace x y = (math_cmd_no_arg "overbrace")^^(exponent x y)
let not_ = (^^) (math_cmd_no_arg "not")
let neq = ne
let to_ = rightarrow

(* Low-Level *)
let atbegindocument = cmd_one_arg "AtBeginDocument"
let addcontentsline toc section name =
  command "addcontentsline" [T,toc; T,section; T,name] T
let vfill = text "\\vfill "
let vfil = text "\\vfil "
let pagestyle = cmd_one_arg "pagestyle"
let thispagestyle = cmd_one_arg "thispagestyle"

(* AMS *)
let black_triangle_left =
  math_cmd_no_arg  ~packages:["amsmath",""] "blacktriangleleft"
let black_triangle_right =
  math_cmd_no_arg ~packages:["amsmath",""] "blacktriangleright"

(*******************************************************************************)

module type BEAMER = sig
  type beamertemplate = [ `NavigationSymbols | `Footline ]
  type tocoptions = [ `CurrentSection | `CurrentSubsection | `HideAllSubsections
  | `HideOtherSubsections | `PauseSections | `PauseSubsections ]

  val frame: ?title: t -> ?subtitle: t -> t -> t
  val setbeamertemplate: beamertemplate -> t -> t

  val insertpagenumber: t
  val insertdocumentendpage: t
  val inserttitle: t
  val insertsection: t
  val insertsubsection: t
  val insertshorttitle: t
  val insertshortsection: t
  val insertshortsubsection: t

  val tableofcontents: tocoptions list -> t
  val at_begin_section: t -> t
  val at_begin_subsection: t -> t
  val at_begin_subsubsection: t -> t
  val block: t -> t -> t (** [block title body] *)

  type color = [
  | `Gray
  | `Red
  | `Green
  | `Blue
  | `Yellow
  | `RGB of float * float * float
  ]

  val color: color -> t -> t

  type overlays_spec = [`I of int]

  (*val command: ?packages: (string * string) list -> string -> ?only: overlays_spec list -> 
    ?opt: (mode * t) -> (mode * t) list -> mode -> t*)
    
  val only: overlays_spec list -> t -> t

  val includegraphics: ?only: overlays_spec list -> t -> t

  type column_alignment = [ `T | `C | `B ]
  val columns: ?align: column_alignment -> t -> t
  val column: ?align: column_alignment -> size -> t -> t
  val equi_columns: ?align: column_alignment -> t list -> t
end

module Beamer = struct
  let frame ?title ?subtitle body =
    let x = concat [
      optcmd "frametitle" title;
      optcmd "framesubtitle" subtitle;
      body;
    ] in
    environment "frame" (T, x) T

  type beamertemplate = [ `NavigationSymbols | `Footline ]

  let setbeamertemplate template body =
    let template = match template with
      | `NavigationSymbols -> "navigation symbols"
      | `Footline -> "footline"
    in
    let template = text template in
    command "setbeamertemplate" [T, template; T, body] T

  let insertpagenumber = command "insertpagenumber" [] T
  let insertdocumentendpage = command "insertdocumentendpage" [] T
  let inserttitle = command "inserttitle" [] T
  let insertsection = command "insertsection" [] T
  let insertsubsection = command "insertsubsection" [] T
  let insertshorttitle = command "insertshorttitle" [] T
  let insertshortsection = command "insertshortsection" [] T
  let insertshortsubsection = command "insertshortsubsection" [] T

  type tocoptions = [ `CurrentSection | `CurrentSubsection | `HideAllSubsections
  | `HideOtherSubsections | `PauseSections | `PauseSubsections ]

  let tableofcontents options =
    let options = make_option T begin function
      | `CurrentSection -> "currentsection"
      | `CurrentSubsection -> "currentsubsection"
      | `HideAllSubsections -> "hideallsubsections"
      | `HideOtherSubsections -> "hideothersubsections"
      | `PauseSections -> "pausesections"
      | `PauseSubsections -> "pausesubsections"
    end options in
    command "tableofcontents" ?opt: options [] T

  let at_begin_section x =
    command "AtBeginSection" [T, x] T

  let at_begin_subsection x =
    command "AtBeginSubsection" [T, x] T

  let at_begin_subsubsection x =
    command "AtBeginSubsubsection" [T, x] T

  let block title body =
    environment "block" ~args: [T, title] (T, body) T

  type color = [
  | `Gray
  | `Red
  | `Green
  | `Blue
  | `Yellow
  | `RGB of float * float * float
  ]

  let color c x =
    match c with
      | `RGB (r, g, b) ->
          let rgb =
            string_of_float r^","^string_of_float g^","^string_of_float b
          in
          within_braces begin concat [
            command "color" ~opt: (A, text "rgb") [A, text rgb] A;
            x
          ] end
      | _ ->
          within_braces begin concat [
            command "color"
              [A, match c with
                 | `Gray -> text "gray"
                 | `Red -> text "red"
                 | `Green -> text "green"
                 | `Blue -> text "blue"
                 | `Yellow -> text "yellow"
                 | `RGB (r, g, b) ->
                     assert false
              ] A;
            x
          ] end

  type overlays_spec = [`I of int]

  let string_of_overlays_spec name = function
    | [] -> name
    | l -> let l = List.map (function `I i -> string_of_int i) l in
      let s = String.concat "," l in
      sprintf "%s<%s>" name s

  let unusual_command ?packages name ?(only=[]) args mode =
    unusual_command ?packages (string_of_overlays_spec name only) args mode
  let command ?packages name ?(only=[]) ?opt args mode = 
    command ?packages (string_of_overlays_spec name only) args mode
    (*TODO A posibility for a command to have a result mode which depend of the mode of its argument*)
  let only only arg = command "only" ~only [A,arg] A

  let includegraphics ?only filename =
    command ?only "includegraphics" [ T, filename ] T

  type column_alignment = [ `T | `C | `B ]
  let letter_of_column_alignment = function
    | `T -> A, text "t"
    | `C -> A, text "c"
    | `B -> A, text "b"
  let columns ?align x =
    environment "columns" ?opt: (Opt.map letter_of_column_alignment align)
      (T, x) T
  let column ?align size x =
    environment "column" ?opt: (Opt.map letter_of_column_alignment align)
      ~args: [A, latex_of_size size] (T, x) T
  let equi_columns ?align cols =
    let count = List.length cols in
    if count <= 0 then error "equi_columns requires at least 1 column";
    let size = `Textwidth (1. /. float_of_int count) in
    let cols = List.map (column size) cols in
    columns ?align (concat cols)
end

module Verbatim = struct
  open Str

  let alphanumplus = regexp "[a-zA-Z0-9]+"
  let ident = regexp "_?[a-zA-Z][a-zA-Z0-9]*\\(_[a-zA-Z0-9]+\\)*"
  let underscore = regexp "_"

  let verbatim s =
    concat begin List.flatten begin List.map begin function
      | Delim s ->
          [ text s ]
      | Text s ->
          let l = ref [] in
          for i = String.length s - 1 downto 0 do
            l := s.[i] :: !l
          done;
          List.map (function
                      | ' ' -> (*text "~"(*command " " [] T*)*)
                          command "hphantom" [T, text " "] T
                      | '\n' -> newline
                      | '\r' -> empty
                      | c -> symbolc c) !l
    end (full_split alphanumplus s) end end

  let rec regexps regapps remapp s =
    match regapps with
      | [] ->
          remapp s
      | (r, a)::rem ->
          concat begin List.map begin function
            | Delim s -> a s
            | Text s -> regexps rem remapp s
          end (full_split r s) end

  let trim_begin chars s =
    let len = String.length s in
    let b = ref 0 in
    while !b < len && List.mem s.[!b] chars do incr b done;
    if !b < len then String.sub s !b (len - !b) else ""

  let trim_end chars s =
    let len = String.length s in
    let e = ref (len-1) in
    while !e >= 0 && List.mem s.[!e] chars do decr e done;
    if 0 <= !e then String.sub s 0 (!e + 1) else ""

  let trim chars s =
    let len = String.length s in
    let b = ref 0 in
    while !b < len && List.mem s.[!b] chars do incr b done;
    let e = ref (len-1) in
    while !e >= 0 && List.mem s.[!e] chars do decr e done;
    if !b <= !e then String.sub s !b (!e - !b + 1) else ""

  let split_lines s =
    Str.split (Str.regexp_string "\n") s

  let pseudocode ?(trim = trim ['\n']) ?(id_regexp = ident)
      ?(kw_apply = textbf)
      ?(id_apply = mathit)
      ?(rem_apply = verbatim)
      ?(keywords = [])
      ?(symbols = [])
      ?(keyword_symbols = [])
      ?(underscore = underscore)
      s =
    let identifier_nosplit kw =
      try List.assoc kw keyword_symbols with Not_found ->
        if List.mem kw keywords then kw_apply (text kw) else
          id_apply (text kw)
    in
    let indexify_identifier id = function
      | [] -> id
      | indexes ->
          let indexes = List.map identifier_nosplit indexes in
          index id (scriptsize (concat (list_insert (text ",") indexes)))
    in
    let s = trim s in
    let ident_regexp =
      (ident,
       fun s ->
         let us_split = split_delim underscore s in
         match us_split with
           | [] -> empty
           | kw::rem ->
               indexify_identifier (identifier_nosplit kw) rem)
    in
    let symbol_regexps =
      List.map (fun (s, l) -> regexp_string s, fun _ -> l) symbols in
    regexps (ident_regexp :: symbol_regexps) rem_apply s

  let keywords ?(apply = textbf) k s =
    regexps [regexp (String.concat "\\|" k), fun x -> apply (verbatim x)]
      verbatim s
end
