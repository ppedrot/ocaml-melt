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

(** LaTeX output. *)

(** LaTeX expressions. *)
type t

(** {2 LaTeX Pervasives} *)

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
  (** The type of LaTeX sizes.
- [`In]: inches
- [`Mm]: millimeters
- [`Cm]: centimeters
- [`Pt]: points (about 1/72 inch)
- [`Em]: approximately the width of an "M" in the current font
- [`Ex]: approximately the width of an "x" in the current font

- [`Pc]: pica (12pt/pc)
- [`Bp]: big pt (72bp/in)
- [`Dd]: didot (1157dd=1238pt)
- [`Cc]: cicero (12dd/cc)
- [`Sp]: scaled point (65536sp/pt)

- [`Parindent]: normal paragraph indentation
- [`Baselineskip]: normal vertical distance between lines in a paragraph
- [`Baselinestretch]: multiplies [`Baselineskip]
- [`Parskip]: the extra vertical space between paragraphs
- [`Textwidth]: the width of text on the page
- [`Linewidth]: width of a line in the local environment
- [`Textheight]: the height of text on the page
- [`Unitlength]: units of length in picture environment

- [`Fill]: rubber length; takes as much space as possible
- [`Stretch]: rubber length; if multiple [`Stretch]-sized commands are issued
  on the same line (or vertical box) they stretch in proportion of their respective
  factor.
*)

val latex_of_size : size -> t
(** Low level function to be used to make new bindings. *)

(** {3 Document} *)

type documentclass = 
    [ `Article | `Report | `Book | `Letter | `Slides | `Beamer | `Custom of string]
type documentoptions = [ `Landscape | `A4paper | `TwoColumn | `Pt of
    int ]

val document:
  ?documentclass: documentclass ->
  ?options: documentoptions list ->
  ?title: t ->
  ?author: t ->
  ?date: t ->
  ?prelude: t ->
  ?packages: (t * t) list -> t -> t
(** The [~packages] argument takes a list of [(name, opt)] where [name] is the
name of the package and [opt] is its option. This is equivalent to
using several calls to [usepackage] in the [~prelude]. *)

(** {3 Variables} *)

(** {4 Basic Constructors} *)

(** Variables are similar to LaTeX counters, except that they are computed
    when pretty-printing the LaTeX AST.
    
    The two basic operations on variables are [get] and [setf].
    [get] outputs an ast depending on the current contents of a
    variable.
    [setf] updates the contents of a variable.

    [get] can also use the contents of a variable at a different
    position in the document.
    To use a position, you need to declare one first with
    [position]. then you can place that position in you document with
    [place]. You must not place a position more than one time.
    If a position isn't placed, the contents of the variables at that
    position will be the default one.

    The final contents of variables is obtained by a fixpoint
    computation wich is performed by the printing functions
    {!to_buffer}, {!to_channel}, {!to_file}, {!to_string}. That
    fixpoint may not terminate. In that case, the log will tell you which
    variable did not converge. *)

type 'a variable

val variable: ?eq:('a -> 'a -> bool) -> ?name:string ->
  ?printer:('a -> string) -> 'a -> 'a variable
(** Declare a new variable.  The last argument is the default value of
    the variable.

    [eq] is the equality function on the type of the variable. Default is [=].

    [name] and [printer] are used to print information when the
    fixpoint calculation diverged. *)

val setf: 'a variable -> ('a -> 'a) -> t
(** Change the value of a variable in the rest of the document. *)

val setf2: 'a variable -> 'b variable -> ('a -> 'b -> 'b) -> t
(** [setf var_in var_out f]
    Change the value of the variable [var_out] in the rest of the document
    using the contents of [var_in]. *)

type position
(** The type of positions in documents. *)

val position: ?name:string -> unit -> position
(** Declare a new position.
    [name] is used to print information when the fixpoint computation
    diverged. *)

val place: position -> t
(** Place a position in the document. *)

val get: ?position:position -> 'a variable -> ('a -> t) -> t
(** Use the contents of a variable to compute part of the document.
    If [get] has no parameter [position] then the current value of the
    variable is taken. Otherwise it is the value at [position]. *)

(** {4 Useful Stuff About Variables} *)

(** All these functions are defined using the above constructors. *)

val set: 'a variable -> 'a -> t
  (** Change the value of a variable.

      [set x v]: return a node which, when evaluated, changes the contents
      of variable [x] to value [v]. *)

val final: 'a variable -> ('a -> t) -> t
  (** Like [get], but the value of the variable is taken at the end of
      the document. *)

val incr_var: int variable -> t
  (** Increment an integer variable.

      [incr_var x] is equivalent to [setf x (fun x -> x + 1)]. *)

val decr_var: int variable -> t
  (** Decrement an integer variable.

      [decr_var x] is equivalent to [setf x (fun x -> x - 1)]. *)

val vari: int variable -> t
  (** Print an integer variable.

      [vari x] is equivalent to [get x (fun x -> text (string_of_int x))]. *)

val varf: float variable -> t
  (** Print a float variable.

      [varf x] is equivalent to [get x (fun x -> text (string_of_float x))]. *)

val varb: bool variable -> t
  (** Print a boolean variable.

      [varb x] is equivalent to [get x (fun x -> text (string_of_bool x))]. *)

val vars: string variable -> t
  (** Print a string variable.

      [vars x] is equivalent to [get x text]. *)

val vart: t variable -> t
  (** Print a variable containing a LaTeX AST.

      [vart x] is equivalent to [get x (fun x -> x)]. *)

val finali: int variable -> t
  (** Print the last value of an integer variable.

      [finali x] is equivalent to [final x (fun x -> text (string_of_int x))]. *)

val finalf: float variable -> t
  (** Print the last value of a float variable.

      [finalf x] is equivalent to [final x (fun x -> text (string_of_float x))]. *)

val finalb: bool variable -> t
  (** Print the last value of a boolean variable.

      [finalb x] is equivalent to [final x (fun x -> text (string_of_bool x))]. *)

val finals: string variable -> t
  (** Print the last value of a string variable.

      [finals x] is equivalent to [final x text]. *)

val finalt: t variable -> t
  (** Print the last value of a variable containing a LaTeX AST.

      [finalt x] is equivalent to [final x (fun x -> x)]. *)

(** {3 References and Labels} *)

(** Example (using the Melt pre-processor):

[let lbl_intro = label ()]

[let intro = section ~label: lbl_intro "This is Section~{ref_ lbl_intro}."] *)

type label

val label: ?name: string -> unit -> label
  (** Declare a new label.

Argument [name]  can be  used to force  the name  of the label  in the
LaTeX file. This can  be useful if you need to refer  to this label in
an external LaTeX  file or if the label itself  is declared in another
LaTeX file. The default value of [name] is ["latex_lib_label_n"] where
[n] is a counter. *)

val ref_: label -> t
  (** Make a reference to the label. *)

(** {3 Figures} *)

type float_position = [ `H | `T | `P | `B | `Force ]
  (** Floating element (figure, ...) positions.
      - [`H]: here
      - [`T]: top of page
      - [`B]: bottom of page
      - [`P]: put on a special page for floats only
      - [`Force]: override internal LaTeX parameters *)
val float_all: float_position list
  (** [[ `H; `T; `B; `P ]] *)

val figure: ?label: label -> ?pos: float_position list -> ?center: bool ->
  ?side_caption: bool -> ?caption: t -> ?wide: bool -> t -> t
  (** Floating figure.

      Default value for [center] is false.
      If [side_caption] is [true], the caption will be placed at the side of
      the figure instead of at the bottom. This uses package [sidecap].
      Default value is [false].

      Argument [~wide: true] must be used for multi-columns documents if you
      want the figure to use the full width of the page. In this case,
      positions [`H] has no effect, and position [`B] adds package
      [stfloats].

      To prevent wide figures from being placed out-of-order with respect to
      their "non-wide" counterparts, use package [fixltx2e]. *)

type wrapfigure_position =
    [ `L | `R | `I | `O | `Force of [ `L | `R | `I | `O ] ]
  (** Figure positions for package [wrapfig].
      - [`L]: left
      - [`R]: right
      - [`I]: inside (if document is twosided)
      - [`O]: outside (if document is twosided)
      - [`Force _]: force the figure to start precisely where specified
(may cause it to run over page breaks) *)

val wrapfigure: ?label: label -> ?pos: wrapfigure_position ->
  ?lines: int -> ?width: size -> ?center: bool -> ?caption: t -> t -> t
  (** Floating figure which makes text wrap around it.

      Uses package [wrapfig].
      Argument [lines] specifies the height of the figure in number of lines.
      It can be useful if LaTeX fails to compute it correctly.
      Default value for [width] is half the text width.
      Default value for [center] is false.

      If there is too much space on top and below the figure, and [lines] does
      not do what you want, you can add
      some negative [vspace]s. In general it is better to let
      LaTeX place the figure for you, though. *)

type floatingfigure_position = [ `L | `R | `P ]
  (** Figure positions for package [floatflt].
      - [`L]: left
      - [`R]: right
      - [`P]: right if the pagenumber is odd, left if even *)

val floatingfigure: ?label: label -> ?pos: floatingfigure_position ->
  ?width: size -> ?center: bool -> ?caption: t -> t -> t
  (** Floating figure which makes text wrap around it.

      Uses package [floatflt].
      Default value for [width] is half the text width.
      Default value for [center] is false. *)

val subfloat: ?label: label -> ?caption: t -> t -> t
  (** Sub-figure.

      Uses package [subfig].
      Use it inside a [figure] to insert sub-figures. *)

(** {3 Miscellaneous Commands} *)

val hyphen: t
  (** Tell LaTeX where to cut words at the end of lines. *)

val index: t -> t -> t
  (** [index x y] produces [{x}_{y}] *)

val exponent: t -> t -> t
  (** [exponent x y] produces [{x}^{y}] *)

val index_exponent: t -> t -> t -> t
  (** [index_exponent x y z] produces [{x}_{y}^{z}].

      This is NOT equivalent to [exponent (index x y) z] as this would
      produce [{{x}_{y}}^{z}]. The former allows the exponent to be printed
      above the index, while the latter does not. *)

val tableofcontents: t
val listoffigures: t
val listoftables: t

val appendix: t

(** [printindex] output an index listing the various point which have
      been referenced by [place_index key]. [key] can be a phrase
      in which case it appears as-is in the index, or some more complex
      instruction (documentation for index
      keys can be found in the Not So Short Introduction to Latex (available
      online) or the Latex Companion).

      If you use at least one of [place_index] or [printindex], a file .idx
      will be produced at the same time as the .aux. It needs to be processed
      by the program makeindex (makeindex file.idx). Then (pdf)latex
      needs to be run again. *)
val place_index: t -> t
val printindex: t

val today: t

val maketitle: t
  (** You should not need [maketitle] if you use {!document}. *)

val part: ?label: label -> t -> t
  (** For the report style. *)

val chapter: ?label: label -> t -> t
val section: ?label: label -> t -> t
val subsection: ?label: label -> t -> t
val subsubsection: ?label: label -> t -> t
val paragraph: t -> t
val par: t
val displaymath: t -> t
val equation: ?label: label -> t -> t
val hfill: t
val vfill: t
val vfil: t
val footnote: t -> t
val latex_of_int: int -> t
val latex_of_float: float -> t
val itemize: t list -> t
val enumerate: t list -> t
val newline: t
  (** Start a new line. *)
val newline_size: size -> t
  (** A newline followed by a vertical space. *)
val newpage: t
  (** Start a new page. *)
val clearpage: t
  (** Same as [newpage], but also force figures and tables floating in the
current page to be printed. *)
val noindent: t
val space : t
  (** Forces a space, same as "\ " in LaTeX *)
val quad: t
val qquad : t
val includegraphics: t -> t
val symbol: int -> t
val symbolc: char -> t
  (** Convert a [char] into an [int] and apply [symbol]. *)

val center: t -> t
val flushleft: t -> t
val flushright: t -> t

val minipage: size -> t -> t

val quote: t -> t
val quotation: t -> t

val stackrel: t -> t -> t

val vspace: size -> t
  (** A vertical space. *)
val hspace: size -> t
  (** An horizontal, possibly negative space. *)
val addvspace: size -> t
  (** Similar to [vspace], but an [addvspace x] followed by an [addvspace y]
will produce an [addvspace] of [max x y]. *)
val ignorespaces: t
  (** Tells LaTeX to ignore following spaces and new lines. Useful at the
      end of a display environment, for instance. *)
val smallskip: t (** A small [vspace]. *)
val medskip: t (** A medium [vspace]. *)
val bigskip: t (** A big [vspace]. *)
val nointerlineskip: t (** Delete the interline vertical space. *)

val phantom: t -> t
  (** Take the space of the argument without actually drawing it *)
val vphantom: t -> t (** Vertical-only phantom *)
val hphantom: t -> t (** Horizontal-only phantom *)

val parbox: size -> t -> t
  (** A box in which new lines and paragraphs may be used. Useful to display
      code listings, for instance. *)

type halignment = [ `C | `L | `R | `S ]
  (** (`C)enter, flush (`L)eft, flush (`R)ight or (`S)pread. *)

val makebox : size -> ?halign:halignment -> t -> t
  (** A box which only deals with horizontaly aligned material. *)
val framebox : size -> ?halign:halignment -> t -> t
  (** Same as [makebox] but draws a frame around the box. *)


type alignment = [ `L | `C | `R ]
type array_column =  [ alignment | `Vert | `Sep of t]
type array_line

val array: array_column list -> array_line list -> t
val array_line: ?sep: size -> ?layout:(int*[alignment|`I]) list -> t list -> array_line
  (** Extra alignment [`I] in layout means that the column inherits the alignment
        of the first corresponding column in the array layout.
        The integers in the layout correspond to over how many of the array's column
        will the cell will span.*)
val array_command : t -> array_line
  (** [array_command x] is a low level command. It gives [x] as an array line to Latex.
         Meant to define alternative commands to draw horizontal lines in arrays.*)
(*spiwack: todo: horizontal line commands, like [hline], [midrule], etc… *)

(* Actually, I don't know what these do. *)
val frontmatter: t
val backmatter: t
val mainmatter: t
val underbrace: t -> t -> t
val overbrace: t -> t -> t

(** {3 Fonts} *)

(** {4 Font Styles} *)

val emph: t -> t (** Emphasize *)

val texttt: t -> t (** Monospace *)
val textsc: t -> t (** Small caps *)
val textit: t -> t (** Italic *)
val textbf: t -> t (** Bold *)
val textrm: t -> t (** Roman *)
val textsf: t -> t (** Sans serif *)

val mathit: t -> t (** Italic (for math mode) *)
val mathbf: t -> t (** Bold (for math mode) *)
val mathrm: t -> t (** Roman (for math mode) *)
val mathsf: t -> t (** Sans serif (for math mode) *)
val mathcal: t -> t (** Caligraphic *)

(** {4 Font Sizes} *)

(** From the smallest to the largest. *)

val tiny: t -> t
val scriptsize: t -> t
val footnotesize: t -> t
val small: t -> t
val normalsize: t -> t
val large: t -> t
val large2: t -> t
val large3: t -> t
val huge: t -> t
val huge2: t -> t

(** {3 Math Accents} *)

val hat: t -> t
val grave: t -> t
val bar: t -> t
val acute: t -> t
val mathring: t -> t
val check: t -> t
val dot: t -> t
val vec: t -> t
val breve: t -> t
val tilde: t -> t
val ddot: t -> t
val widehat: t -> t
val widetilde: t -> t

(** {3 Greek Letters} *)

(** {4 Lowercase} *)

val alpha: t
val beta: t
val gamma: t
val delta: t
val epsilon: t
val varepsilon: t
val zeta: t
val eta: t
val theta: t
val vartheta: t
val iota: t
val kappa: t
val varkappa: t
val lambda: t
val mu: t
val nu: t
val xi: t
val pi: t
val varpi: t
val rho: t
val varrho: t
val sigma: t
val varsigma: t
val tau: t
val upsilon: t
val phi: t
val varphi: t
val chi: t
val psi: t
val omega: t

val digamma: t

(** {4 Uppercase} *)

val gamma_: t
val delta_: t
val theta_: t
val lambda_: t
val xi_: t
val pi_: t
val sigma_: t
val upsilon_: t
val phi_: t
val psi_: t
val omega_: t

(** {3 Hebrew Letters} *)

val aleph: t
val beth: t
val gimel: t
val daleth: t

(** {3 Mathematical Symbols} *)

(** {4 Binary Relations} *)

val le: t (** less or equal *)
val leq: t (** less or equal (same as {!le}) *)
val leqslant: t (** less or equal (with equal bar parallel to the 'less than' sign *)
val ge: t (** greater or equal *)
val geq: t (** greater or equal (same as {!ge}) *)
val geqslant: t (** greater or equal (with equal bar parallel to the 'less than' sign *)
val equiv: t (** = with 3 bars *)
val ll: t (** << *)
val gg: t (** >> *)
val doteq: t (** = with . on top *)
val prec: t (** trumpet < *)
val succ: t (** trumpet > *)
val sim: t (** ~ *)
val preceq: t (** trumpet < or equal *)
val succeq: t (** trumpet > or equal *)
val simeq: t (** ~ or equal *)
val subset: t
val supset: t
val approx: t (** double ~ *)
val subseteq: t
val supseteq: t
val cong: t (** = with ~ on top *)
val sqsubset: t (** square strict subset (latexsym package) *)
val sqsupset: t (** square strict superset (latexsym package) *)
val join_: t (** small bowtie (latexsym package) *)
val sqsubseteq: t (** square subset or equal *)
val sqsupseteq: t (** square superset or equal *)
val bowtie: t
val in_: t (** in set *)
val owns: t (** inverted in set *)
val propto: t (** infinite with open right buckle *)
val vdash: t (** |- *)
val dashv: t (** -| *)
val models: t (** |= *)
val mid: t (** | *)
val parallel: t (** || *)
val perp: t (** _|_ *)
val smile: t
val frown: t
val asymp: t (** frown with smile on top *)
val not_: t -> t (** generic negation of binary symbol. [not_ in_] will print as ∉ *)
val notin: t (** not in set (∉) *)
val ne: t (** not equal (≠)*)
val neq: t (** not equal (same as {!ne}) *)

(** {4 Binary Operators} *)

val pm: t (** - with + on top (∓) *)
val mp: t (** + with - on top (±)*)
val triangleleft: t (** ◃ *)
val cdot: t (** centered . *)
val div: t (** - with . on top and . on the bottom (÷)*)
val triangleright: t (** ▹ *)
val times: t (** × *)
val setminus: t (** backslash *)
val star: t (** 5-branches star *)
val cup: t (** set union *)
val cap: t (** set intersection *)
val ast: t (** asterisk * (6-branches star) *)
val sqcup: t (** square cup *)
val sqcap: t (** square cap *)
val circ: t (** a small circle *)
val lor_: t (** \/ *)
val land_: t (** /\ *)
val bullet: t (** a small filled circle *)
val oplus: t (** a circle with a + inside *)
val ominus: t (** a circle with a - inside *)
val diamond: t (** a small square rotated 45 degrees *)
val odot: t (** a circle with a centered . inside *)
val oslash: t (** a slashed circle *)
val uplus: t (** a cup with a + inside *)
val otimes: t (** a crossed circle *)
val bigcirc: t
val amalg: t
val bigtriangleup: t
val bigtriangledown: t
val dagger: t
val lhd: t (** bigger [triangleleft] (latexsym package) *)
val rhd: t (** bigger [triangleright] (latexsym package) *)
val ddagger: t (** double dagger ([dagger] with one more cross on the bottom) *)
val unlhd: t (** bigger, underlined [triangleleft] (latexsym package) *)
val unrhd: t (** bigger, underlined [triangleright] (latexsym package) *)
val wr: t (** a vertical ~ *)

(** {4 BIG Operators} *)

val sum: t
val prod: t
val coprod: t
val bigcup: t
val bigcap: t
val bigvee: t
val bigwedge: t
val bigsqcup: t
val biguplus: t
val int: t
val oint: t
val bigodot: t
val bigoplus: t
val bigotimes: t

(** {4 Arrows} *)

val leftarrow: t (** <- *)
val rightarrow: t (** -> *)
val to_: t (** -> (same as {!rightarrow}) *)
val leftrightarrow: t (** <-> *)
val leftarrow_: t (** <= *)
val rightarrow_: t (** => *)
val leftrightarrow_: t (** <=> *)
val longleftarrow: t (** <-- *)
val longrightarrow: t (** --> *)
val longleftrightarrow: t (** <--> *)
val longleftarrow_: t (** <== *)
val longrightarrow_: t (** ==> *)
val longleftrightarrow_: t (** <==> *)
val iff: t (** <==> (bigger spaces) *)

val mapsto: t
val longmapsto: t
val hookleftarrow: t
val hookrightarrow: t
val leftharpoonup: t
val rightharpoonup: t
val leftharpoondown: t
val rightharpoondown: t
val rightleftharpoons: t
val uparrow: t
val downarrow: t
val updownarrow: t
val uparrow_: t (** double [uparrow] *)
val downarrow_: t (** double [downarrow] *)
val updownarrow_: t (** double [updownarrow] *)
val nearrow: t (** North-East arrow *)
val searrow: t (** South-East arrow *)
val swarrow: t (** South-West arrow *)
val nwarrow: t (** North-West arrow *)
val leadsto: t (** ~> (latexsym package) *)

(** {4 Symbols to be Sorted (Stay Tuned)} *)

val box_: t
  (** A square box, for instance to end proofs (QED).
      Adds package [latexsym]. *)

val langle: t (** ⟨ *)
val rangle: t (** ⟩ *)
val lceil: t (** ⌈ *)
val rceil: t (** ⌉ *)

val frac: t -> t -> t

val land_: t (** /\ *)
val lor_: t (** \/ *)
val lnot: t (** ¬ *)
val neg: t (** ¬ (like {!lnot}) *)
val forall: t (** ∀ *)
val exists: t (** ∃ *)

val top : t (** ⊤ *)
val bot : t (** ⊥ *)

val sharp : t

val dots: t
val cdots: t (** Centered dots [...] *)
val ldots: t (** elipsis, works in math and text mode *)

val emptyset: t

type doublable_delimiter =
    [ `Down | `Up | `Up_down | `Vert ]
type delimiter =
    [ `None | `Brace | `Paren | `Bracket | `Angle | `Floor | `Ceil | `Slash
    | doublable_delimiter | `Double of doublable_delimiter ]

val left: delimiter -> t
val right: delimiter -> t

val just_left: delimiter -> t -> t
  (** [just_left d x]: concatenation of [left d], [x] and [right `None]. *)

val just_right: delimiter -> t -> t
  (** [just_right d x]: concatenation of [left `None], [x] and [right d]. *)

val between: delimiter -> t -> t
  (** [between d x]: concatenation of [left d], [x] and [right d]. *)

val oe: t (** French e in o as in "coeur", "noeud"... *)

(** {4 AMS} *)

val mathbb: t -> t
val mathfrak: t -> t
val align : t -> t
(** the AMS align environment to align equations using & *)
val align_ : t -> t
(** same as [align], but without numbering *)

val gather : t -> t
val gather_ : t -> t
val split : t -> t
val proof : ?opt:t -> t -> t

val twoheadrightarrow : t (** ->> *)
val square: t

val par_: t (** The paragraph symbol. *)

val black_triangle_left: t
val black_triangle_right: t

(** {4 Mathpartir} *)

val mathpar : t list -> t
  (** Math paragraph.
This function inserts [and] commands between each item to split them. *)

val inferrule : ?lab: t -> ?left: t -> ?right: t -> ?vdots: size ->
  ?width: size -> ?leftskip: size -> ?rightskip: size -> t list -> t list -> t
  (** Inference rule.
[inferrule pre post] builds an inference rule with [pre] at the top and [post]
at the bottom. If [pre] or [post] is empty, the bar is not drawn.
    @param lab label to put above the rule
    @param left label to put on the left of the rule
    @param right label to put on the right of the rule
    @param vdots raise the rule and draw vertical dots ; the length argument
is translated to a number of line-skips *)

(** {4 Saint Mary Road} *)

(** The package ["stmaryrd"] is automatically added by these commands. *)

val llbracket: t
val rrbracket: t

(** {3 Slide Document Class} *)

val slide: t -> t

(** {3 Beamer Document Class} *)

module type BEAMER = sig
  type beamertemplate = [ `NavigationSymbols | `Footline ]
  type tocoptions = [ `CurrentSection | `CurrentSubsection | `HideAllSubsections
  | `HideOtherSubsections | `PauseSections | `PauseSubsections ]

  val frame: ?title: t -> ?subtitle: t -> t -> t
    (** One slide. *)

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
    (** Same as {!Latex.includegraphics} but with the [only] parameter. *)

  (** {2 Columns} *)

  type column_alignment = [ `T | `C | `B ]
    (** Vertical alignment of a column: top, center or bottom. *)

  val columns: ?align: column_alignment -> t -> t
    (** Put your columns in this environment.

        The [align] argument is the default vertical alignment for each
        column.

        The last argument must be a concatenation of several {!column}s. *)

  val column: ?align: column_alignment -> size -> t -> t
    (** One column.

        Columns must be put inside the {!columns} environment. *)

  val equi_columns: ?align: column_alignment -> t list -> t
    (** Several columns with the same size each.

        The size of each column is [`Textwidth (1. /. c)] where [c] is the
        length of the list. The [align]ment is the same for each column.

        Example with two columns: [equi_columns ["Hello"; "World"]]*)
end

module Beamer : BEAMER

(** {3 Verbatim Modes} *)

module Verbatim: sig
  val verbatim: string -> t
    (** Replace all non-alphanumerical characters by an application of the
[symbol] command, all spaces by escaped spaces, and all new lines
by actual new lines. *)

  val regexps: (Str.regexp * (string -> t)) list -> (string -> t) -> string -> t
    (** [regexps [r1, a1; r2, a2; ...] f s]: apply [a1] on all matches of [r1]
in [s], then [a2] on all matches of [r2], and so on.
Note that [r2] is only tested on the parts of [s] which do not match [r1].
[f] is applied on the parts of [s] which are not matched by any of the
regular expressions. *)

  val keywords: ?apply: (t -> t) -> string list -> string -> t
    (** [keywords k s]: apply [verbatim] on [s] but also apply [~apply] on all
  keywords given in [k]. The default value of [~apply] is [textbf] (bold
  font).

  [keywords ["let"; "in"]] is the same as
  [regexps [Str.regexp "let\\|in", fun x -> textbf (verbatim x)] verbatim]. *)

  val pseudocode:
    ?trim: (string -> string) ->
    ?id_regexp: Str.regexp ->
    ?kw_apply: (t -> t) ->
    ?id_apply: (t -> t) ->
    ?rem_apply: (string -> t) ->
    ?keywords: string list ->
    ?symbols: (string * t) list ->
    ?keyword_symbols: (string * t) list ->
    ?underscore: Str.regexp ->
    string -> t
    (** Pseudocode parsing.
        @param trim apply this function first (default is [trim ['\n']])
        @param id_regexp the regular expression used to parse identifiers,
including keywords (default is words starting with a letter or an underscore
followed by any number of letter or digit, followed by any number
of groups of underscore followed by at least one letter or digit:
[Str.regexp "[_a-zA-Z][a-zA-Z0-9]*\\(_[a-zA-Z0-9]+\\)*"])
        @param kw_apply applied to keywords (default is [textbf])
        @param id_apply applied to identifiers (default is [mathit])
        @param rem_apply applied to remaining parts (default is [verbatim])
        @param keywords keyword list 
        @param symbols symbol list and the way they are printed
        @param keyword_symbols keyword list that should be printed in a special
way, as symbols, but parsed as identifiers
        @param underscore delimiter used to split identifiers
(default is underscore (['_']))

Keywords, keyword symbols and identifiers are split using
[underscore] as delimiter.
The first part is replaced by the corresponding [Latex.t].
The other parts are displayed as indexes separated by commas ([',']).
They are also treated as identifiers, potentiel keywords or keyword symbols. *)

  (** {2 Tools to Build Modes} *)

  val trim: char list -> string -> string
    (** Delete characters at the beginning and at the end of a string.
        [trim [' '; '\n'] s] will return a copy of s without spaces and
new lines at the beginning and at the end. *)

  val trim_begin: char list -> string -> string
    (** Delete characters at the beginning of a string.
        [trim [' '; '\n'] s] will return a copy of s without spaces and
new lines at the beginning. *)

  val trim_end: char list -> string -> string
    (** Delete characters at the end of a string.
        [trim [' '; '\n'] s] will return a copy of s without spaces and
new lines at the end. *)

  val split_lines: string -> string list
    (** Split a string according to the ['\n'] delimiter, which is not kept. *)
end

(** {2 Low-Level LaTeX} *)

(** LaTeX mode: math, text or any. *)
type mode = M | T | A

(** {3 Constructors} *)

val empty: t
  (** The empty LaTeX tree.

      Equivalent to [concat []] or [text ""]. *)

val is_empty: t -> bool
  (** Test whether a LaTeX tree is empty.

      A concatenation of empty trees is also empty.

      A tree containing a {!set} node is not empty.

      A tree containing {!get} or {!final} nodes is
      not empty, even if the call will produce an empty tree when
      evaluating variables. *)

(** Raw LaTeX. *)
val text: string -> t

(** Concatenation. *)
val concat: t list -> t

(** Infix Concatenation. *)
val (^^): t -> t -> t

(** LaTeX Command. *)
val command: ?packages: (string * string) list -> string -> ?opt: (mode * t) ->
  (mode * t) list -> mode -> t
(** [command name args mode] produces the LaTeX command [name] applied to
arguments [args].

The command should be used in mode [mode]. For exemple,
the [ensuremath] LaTeX command should be used in math mode. The command will
be coerced using [mbox] or [$ ... $] if [mode] differs from the mode it is
used in.

The [opt] optional parameter may be used to provide an optional parameter
(in brackets [[]]) to the LaTeX command.

Arguments [opt] and [args] must be given with their expected mode and will
be coerced if needed. For example, the [mbox] command expect an argument in
text mode (the argument must be coerced using [$ ... $] if it is math).
The [ensuremath] command expects an argument in any mode.

All packages [(name, opt)] given using [packages] will automatically be used by
[document]. *)

type arg_kind
  val bracket : arg_kind
  val brace : arg_kind
  val nobr : arg_kind
val unusual_command : ?packages: (string * string) list -> string -> 
  (mode * arg_kind * t) list -> mode -> t
(** [unusual_command] does the same as [command], but is more low level.
      Instead of having a single optional argument and a list of mandatory
      arguments, it only has a list of arguments.

      Each argument comes not only with its content and mode, but with an
      "argument kind" (type [arg_kind]) specifying whether it is a brace
      argument (corresponding to mandatory arguments in [command]) or a bracket
      argument (corresponding, in turn, to the option argument of [command]).

      This allows to handle commands which have several optional arguments, 
      or where optional and mandatory arguments are interleaved. *)

val within_braces: t -> t
  (** [within_braces x] produces [{x}].
      Typically meant to be used together with [unusual_command]. *)

(** LaTeX Environment. *)
val environment: ?packages: (string * string) list -> string ->
  ?opt: (mode * t) -> ?args: (mode * t) list -> (mode * t) -> mode -> t
(** Same as function [command], except that it only takes one argument
(the environment body) and produces an environment, i.e. using the
[begin] and [end] commands. The [args] parameters may be used to give
additional arguments, such as the columns of an array.

All packages [(name, opt)] given using [packages] will automatically be used by
[document]. *)

(** Ensure text or math mode. *)
val mode: mode -> t -> t
(** [mode m x] returns [x] if its mode is already [m]. If its mode is not [m],
the result is [x] coerced using [mbox] or [$ ... $]. *)

(** {3 Basic blocks to make custom [document] functions} *)

val documentclass : ?opt:(mode*t) -> t -> t
  (** All document must start with a single document class declaration,
optionnally with arguments. [documentclass cls] means that [cls]
(represented as a [Latex.t]) is the class of the document. The optional
argument is given as a [Latex.t] as well, for generality. *)

val required_packages : t
  (** Your prelude must contain the list of packages required by your
document. That is a single occurence of [required_packages]. Note that
it does not make sense out of the document's prelude. *)

val require_packages : (t*t) list -> t
  (** [require_packages] takes as argument a list of pairs
[package,option]. Each [package] is required (see {!packages}) with
option [option]. The argument [~packages] of {!document} is implemented
as a [require_package]. This command can be used anywhere in a document,
if needed. *)

val documentmatter : t -> t
  (** [documentmatter body] renders your actual document, [body],
according to the rules specified in the prelude. It is simply
LaTeX's [document] command. *)

(** {3 Miscellaneous} *)

val latex: t
  (** "LaTeX" written in a fancy but official way. *)

val usepackage: ?opt: t -> t -> t
  (** You can use this in the [~prelude] of your [document], but it is better
to use the [~packages] argument of [document]. Note that some commandes
add their own packages to the document automatically. *)

val input: t -> t
  (** Include a LaTeX file. Usually you'd prefer to open an OCaml module,
but this can be useful if you have a [.tex] file with macros that you want
to reuse. *)

val newcommand: int -> t -> t -> t
(** [newcommand parameter_count name body] defines a new command with
[parameter_count] arguments, where you can use the [i]th argument by writing
[#i] in the body, just as in Latex. Normally you'd prefer to just define
an OCaml value with [let]. *)

val renewcommand: int -> t -> t -> t
(** Same as [newcommand] except that it can redefine existing LaTeX commands. *)

val block: t -> t
  (** [block x] produces [{x}]. Should only be used in some rare cases when
      you want to be very precise about what LaTeX should do.
      If [x] is empty, the braces are not added. If you need braces even if
      [x] is empty, use {!within_braces}. *)

val place_label: label -> t
  (** [place_label lbl] places label [lbl]. Normally you would prefer using
the various [~label] optional arguments available, and only use [place_label]
for unimplemented features or if you are feeling hackish. *)

val atbegindocument: t -> t
val addcontentsline: t -> t -> t -> t
  (** [addcontentsline toc section name] *)

val pagestyle: t -> t
val thispagestyle: t -> t

(** {2 Printing} *)

type env
(* environment used to keep track of the content of variables between
   multiple applications of to_* functions *)

val get_in_env: 'a variable -> env -> 'a

(** All printing functions take the expected mode as a parameter
(default is text). The printed expression will be coerced if its mode differs. *)

val to_buffer: ?mode: mode -> ?env: env -> Buffer.t -> t -> env
val to_channel: ?mode: mode -> ?env: env -> out_channel -> t -> env
val to_file: ?mode: mode -> ?env: env -> string -> t -> env
val to_string: ?mode: mode -> t -> string
val to_string_with_env: ?mode: mode -> ?env: env -> t -> string * env
