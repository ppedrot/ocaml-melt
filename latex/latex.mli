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

- [`Fill]: rubber length; take as much space as possible
*)

val latex_of_size : size -> t
(** Low level function to be used to make new bindings. *)

(** {3 Document} *)

type documentclass = 
    [ `Article | `Report | `Book | `Letter | `Slides | `Beamer | `Custom of string]
type documentoptions = [ `Landscape | `A4paper ]

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

    Each time you print a value of type {!t} using {!to_buffer}, {!to_channel},
    {!to_file}, {!to_string} or using the [emit] function of the Melt library,
    variables are re-initialized. They are then computed in the order they
    appear in the tree. *)

type 'a variable

val variable: 'a -> 'a variable
  (** Declare a new variable.

      The argument is the initial value of the variable, i.e. the value of
      the variable at the beginning of the pretty-printed AST (usually
      the beginning of the document). *)

val set: 'a variable -> 'a -> t
  (** Change the value of a variable.

      [set x v]: return a node which, when evaluated, changes the contents
      of variable [x] to value [v]. *)

val get: 'a variable -> ('a -> t) -> t
  (** Get the current value of a variable.

      [get x f]: return a node which depends on the value [v] of variable [x]
      at the moment the node is evaluated. The resulting node is [f v].

      Function [f] may itself call [set], [get] or [final] in its body. *)

val final: 'a variable -> ('a -> t) -> t
  (** Get the final value of a variable.

      [final x f]: return a node which depends on the final value of
      variable [x]. This is similar to [get], except that the resulting node
      is [f v] where [v] is the value of [x] at the end of the evaluation
      of the AST which is being printed.

      If you print your document using multiple calls to {!to_buffer},
      {!to_channel}, {!to_file}, {!to_string} or [Melt.emit], final
      values will actually be intermediate values between each of these calls.

      Function [f] may not call [set] nor [get].
      It can call [final], however. *)

val reinitialize_variables: unit -> unit
  (** Reinitialize all variables.

      All variables are set to the value which was given at their creation.
      Call this function if you emit multiple documents in the same program.
      The [Melt.emit] function also calls [reinitialize_variables]. *)

(** {4 Useful Stuff About Variables} *)

(** All these functions are defined using the above constructors. *)

val setf: 'a variable -> ('a -> 'a) -> t
  (** Apply a function to a variable.

      [setf x f] is equivalent to [get x (fun v -> set x (f v))]. *)

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
  (** [index x y] produces [{x}^{y}] *)

val index_exponent: t -> t -> t -> t
  (** [index_exponent x y z] produces [{x}_{y}^{z}].

      This is NOT equivalent to [exponent (index x y) z] as this would
      produce [{{x}_{y}}^{z}]. The former allows the exponent to be printed
      above the index, while the latter does not. *)

val tableofcontents: t
val listoffigures: t
val listoftables: t

val appendix: t

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

val minipage: size -> t -> t

val quote: t -> t

val stackrel: t -> t -> t

val vspace: size -> t
  (** A vertical space. *)
val hspace: size -> t
  (** An horizontal, possibly negative space. *)
val addvspace: size -> t
  (** Similar to [vspace], but an [addvspace x] followed by an [addvspace y]
will produce an [addvspace] of [max x y]. *)
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

type array_column = [ `L | `C | `R | `Vert | `Sep of t ]
type array_line

val array: array_column list -> array_line list -> t
val array_line: ?sep: size -> t list -> array_line

(* Actually, I don't know what these do. *)
val neg: t
val not_: t -> t
val mathfrak: t -> t
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

(** {3 Mathematical Symbols} *)

(** {4 Binary Relations} *)

val le: t (** lesser or equal *)
val leq: t (** lesser or equal (same as {!le}) *)
val ge: t (** greater or equal *)
val geq: t (** greater or equal (same as {!ge}) *)
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
val notin: t (** not in set *)
val ne: t (** not equal *)
val neq: t (** not equal (same as {!ne}) *)

(** {4 Binary Operators} *)

val pm: t (** - with + on top *)
val mp: t (** + with - on top *)
val triangleleft: t
val cdot: t (** centered . *)
val div: t (** - with . on top and . on the bottom *)
val triangleright: t
val times: t
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

val langle: t
val rangle: t
val lceil: t
val rceil: t

val frac: t -> t -> t

val land_: t (** /\ *)
val lor_: t (** \/ *)
val lnot: t
val forall: t
val exists: t

val top : t
val bot : t

val sharp : t

val dots: t
val cdots: t (** Centered dots [...] *)
val ldots: t

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
val align : t -> t
(** the AMS align environment to align equations using & *)
val align_ : t -> t
(** same as [align], but without numbering *)

val gather : t -> t
val gather_ : t -> t
val split : t -> t
val proof : ?opt:t -> t -> t

val twoheadrightarrow : t (** ->> *)

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

(** {3 Miscellaneous} *)

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

(** All printing functions take the expected mode as a parameter
(default is text). The printed expression will be coerced if its mode differs. *)

val to_buffer: ?mode: mode -> Buffer.t -> t -> unit
val to_channel: ?mode: mode -> out_channel -> t -> unit
val to_file: ?mode: mode -> string -> t -> unit
val to_string: ?mode: mode -> t -> string
