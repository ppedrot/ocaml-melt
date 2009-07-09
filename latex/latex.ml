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
end

type mode = M | T | A
type arg_kind = Bracket | Brace

type t =
  | Command of (string * string) list * string *
      (mode * arg_kind * t) list * mode
  | Text of string
  | Environment of (string * string) list * string * (mode * t) option *
      (mode * t) list * (mode * t) * mode
  | Concat of t list
  | Mode of mode * t

let unusual_command ?(packages = []) name args mode =
  Command(packages, name, args, mode)
let command ?(packages=[]) name ?opt args mode =
    let opt = Opt.map (fun (m,t) -> (m,Bracket,t)) opt in
    let args = List.map (fun (m,t) -> (m,Brace,t)) args in
    let args = Opt.cons opt args in
    unusual_command ~packages name args mode


let text s = Text s
let environment ?(packages = []) name ?opt ?(args = []) body mode =
  Environment(packages, name, opt, args, body, mode)
let concat l = Concat l
let (^^) x y = concat [x; y]
let mode mode x = Mode(mode, x)

let braced x = concat [
  text "{";
  x;
  text "}";
]

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
let rec out toplevel mode pp = function
  | Command(_, "par", [], T) when toplevel ->
      Pp.bol pp;
      Pp.newline ~force: true pp
  | Command(_, name, args, rm) ->
      ensure_mode pp rm mode begin fun () ->
        if args = [] && mode = T then Pp.char pp '{';
        Pp.char pp '\\';
        Pp.string pp name;
        if args = [] && mode = M then Pp.space pp;
        if args = [] && mode = T then Pp.char pp '}';
        out_args pp args
      end
  | Environment(_, name, opt, args, (bodymode, body), rm) ->
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
  | Concat l ->
      List.iter (out toplevel mode pp) l

and command_argument pp (mode, x) before after =
  Pp.string pp before;
  out false mode pp x;
  Pp.string pp after

and command_argument_braces pp ca = command_argument pp ca "{" "}"

and command_argument_brackets pp ca = command_argument pp ca "[" "]"

and out_args =
  let out_arg pp = function
    | (m,Bracket,t) -> command_argument_brackets pp (m,t)
    | (m,Brace,t) -> command_argument_braces pp (m,t)
  in
  fun pp args ->
  List.iter (out_arg pp) args

let to_buffer ?(mode = T) buf x = out true mode (Pp.make buf) x

let to_channel ?mode c x =
  let buf = Buffer.create 69 in
  to_buffer ?mode buf x;
  Buffer.output_buffer c buf

let to_file ?mode f x =
  let oc = open_out f in
  to_channel ?mode oc x;
  close_out oc

let to_string ?mode x =
  let buf = Buffer.create 69 in
  to_buffer ?mode buf x;
  Buffer.contents buf

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

let latex_of_size size = text (string_of_size size)

let latex_of_int x = text (string_of_int x)

let empty = concat []

let rec none_if_empty x = match x with
  | Text "" | Concat [] -> None
  | Concat l ->
      List.fold_left begin fun acc -> function
        | None -> None
        | Some _ -> acc
      end (Some x) (List.map none_if_empty l)
  | Mode(_, y) ->
      if none_if_empty y = None then None else Some x
  | Command _ | Environment _ | Text _ -> Some x

let optcmd name = function
  | Some arg -> command name [T, arg] T
  | None -> empty

let labelo l = optcmd "label" (Opt.map text l)

let par = command "par" [] T

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

(*******************************************************************************)

module PackageSet = Set.Make(struct
  type u = t * t
  type t = u
  let compare = compare
end)

let packageset_of_list ?(acc = PackageSet.empty) =
  List.fold_left (fun acc p -> PackageSet.add p acc) acc

let rec packages_used acc = function
  | Text _ ->
      acc
  | Command(packs, _, params, _) ->
      let packs = List.map (fun (x, y) -> text x, text y) packs in
      let acc = packageset_of_list ~acc packs in
      let params = List.map (fun (_,_,x) -> x) params in
      List.fold_left packages_used acc params
  | Environment(packs, _, Some(_, opt), params, body, _) ->
      let packs = List.map (fun (x, y) -> text x, text y) packs in
      let acc = packageset_of_list ~acc packs in
      let params = body::params in
      List.fold_left packages_used (packages_used acc opt) (List.map snd params)
  | Environment(packs, _, None, params, body, _) ->
      let packs = List.map (fun (x, y) -> text x, text y) packs in
      let acc = packageset_of_list ~acc packs in
      let params = body::params in
      List.fold_left packages_used acc (List.map snd params)
  | Concat l ->
      List.fold_left packages_used acc l
  | Mode(_, x) ->
      packages_used acc x

(*******************************************************************************)

type documentclass = [ `Article | `Report | `Book | `Letter | `Slides | `Beamer ]
type documentoptions = [ `Landscape | `A4paper ]

let usepackage ?opt name =
  let opt = Opt.map (fun x -> T, x) opt in
  command "usepackage" ?opt [T, name] T

let input file = command "input" [T,file] T

let newcommand count name body = 
  command "newcommand"
    ?opt: (if count = 0 then None else Some(T, latex_of_int count))
    [T, name; T, body] T

let renewcommand count name body = 
  command "renewcommand"
    ?opt: (if count = 0 then None else Some(T, latex_of_int count))
    [T, name; T, body] T

let document ?(documentclass=`Article) ?(options=[]) ?title ?(author = empty)
    ?date ?(prelude=empty) ?(packages=[]) body =
  let packages =
    PackageSet.elements (packages_used (packageset_of_list packages) body) in
  let dc = match documentclass with
    | `Article -> "article"
    | `Report -> "report"
    | `Book -> "book"
    | `Letter -> "letter"
    | `Slides -> "slides"
    | `Beamer -> "beamer"
  in
  let options = make_option T begin function
    | `Landscape -> "landscape"
    | `A4paper -> "a4paper"
  end options in
  let body = if title <> None then command "maketitle" [] T ^^ body else body in
  concat [
    command "documentclass" ?opt: options [T, text dc] T;
    par;
    concat begin
      List.map (fun (n, o) -> usepackage ?opt: (none_if_empty o) n) packages
    end;
    par;
    prelude;
    par;
    optcmd "title" title;
    command "author" [T, author] T;
    optcmd "date" date;
    par;
    environment "document" (T, body) T;
  ]

let block x = match none_if_empty x with
  | None -> empty
  | Some x -> text "{" ^^ x ^^ text "}"
let index x y = mode M (block x ^^ text "_" ^^ block y)
let exponent x y = mode M (block x ^^ text "^" ^^ block y)

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

let emph x = command "emph" [T, x] T

let texttt x = command "texttt" [T, x] T
let textsc x = command "textsc" [T, x] T
let textit x = command "textit" [T, x] T
let textbf x = command "textbf" [T, x] T
let textrm x = command "textrm" [T, x] T
let textsf x = command "textsf" [T, x] T

let mathit x = command "mathit" [M, x] M
let mathbf x = command "mathbf" [M, x] M
let mathcal x = command "mathcal" [M, x] M

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

type array_column = [ `L | `C | `R | `Vert | `Sep of t]

type array_line = {
  al_columns: t list;
  al_sep: size option;
}

let array_line ?sep x = {
  al_columns = x;
  al_sep = sep;
}

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

let includegraphics filename = command ~packages: ["graphicx", ""] "includegraphics" [ T, filename ] T

let symbol i = command "symbol" [T, latex_of_int i] T
let symbolc c = symbol (Char.code c)

type float_position = [ `H | `T | `P | `B ]

let float_all : float_position list = [ `H ; `T ; `P ; `B ]

let figure ?label ?(pos = [ `H ]) ?(center = false) ?caption body =
  let pos = String.concat "" begin List.map begin function
    | `H -> "h"
    | `T -> "t"
    | `P -> "p"
    | `B -> "b"
  end pos end in
  let body = if center then command "centering" [] T ^^ body else body in
  let body = match caption with
    | None -> body
    | Some caption -> body ^^ command "caption" [T, caption] T
  in
  let body = body ^^ labelo label in
  environment ~opt: (T, text pos) "figure" (T, body) T

let minipage size x =
  environment ~args: [A, latex_of_size size] "minipage" (T, x) T

let center x = environment "center" (T, x) T

let array c l =
  let cols = concat begin List.map begin function
    | `L -> text "l"
    | `C -> text "c"
    | `R -> text "r"
    | `Vert -> text "|"
    | `Sep t -> concat [ text "@{" ; t ; text "}"] 
  end c end in
  let numcols = List.length (List.filter (function `L | `C | `R -> true | _ -> false) c) in
  let lines = List.map begin fun al ->
    let lc = al.al_columns in
    if List.length lc <> numcols then
      failwith (sprintf "array: line with %d columns instead of %d"
                  (List.length lc) (List.length c));
    concat (list_insert (text " & ") lc) ^^ newlinegen al.al_sep
  end l in
  let body = concat lines (*(list_insert newline lines)*) in
  environment "array" ~args: [M, cols] (M, body) M

let list_env l name = 
(*  let items = List.map ((^^) (command "item" [] T)) l in*)
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

type delimiter = [ `None | `Brace | `Paren | `Vert | `Bracket ]

let left d =
  let ds = match d with
    | `None -> "."
    | `Brace -> "\\{"
    | `Paren -> "("
    | `Vert -> "\\|"
    | `Bracket -> "["
  in
  command (sprintf "left%s" ds) [] M

let right d =
  let ds = match d with
    | `None -> "."
    | `Brace -> "\\}"
    | `Paren -> ")"
    | `Vert -> "\\|"
    | `Bracket -> "]"
  in
  command (sprintf "right%s" ds) [] M

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

(*******************************************************************************)

let le = command "le" [] M
let ge = command "ge" [] M
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
let lhd = command "lhd" [] M
let rhd = command "rhd" [] M
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

let mathbb x = command "mathbb" [M, x] M
let align x = environment "align" (M, x) T
let align_ x = environment "align*" (M, x) T
let gather x = environment "gather" (M, x) T
let gather_ x = environment "gather*" (M, x) T
let split x = environment "split" (M,x) M

let proof ?opt t = 
  let opt = Opt.map (fun x -> T, x) opt in
  environment "proof" ?opt (T, t) T

let twoheadrightarrow = command "twoheadrightarrow" [] M

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

  type color = [ `Gray | `Red | `Green | `Blue | `Yellow ]

  val color: color -> t -> t

  type overlays_spec = [`I of int]

  (*val command: ?packages: (string * string) list -> string -> ?only: overlays_spec list -> 
    ?opt: (mode * t) -> (mode * t) list -> mode -> t*)
    
  val only: overlays_spec list -> t -> t

  val includegraphics: ?only: overlays_spec list -> t -> t

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

  type color = [ `Gray | `Red | `Green | `Blue | `Yellow ]

  let color c x =
    braced begin concat [
      command "color"
        [A, match c with
           | `Gray -> text "gray"
           | `Red -> text "red"
           | `Green -> text "green"
           | `Blue -> text "blue"
           | `Yellow -> text "yellow"
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

  let includegraphics ?only filename = command ?only "includegraphics" [ T, filename ] T


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
