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

(** LaTeX mode: math, text or any. *)
type mode = M | T | A

(** {2 Constructors} *)

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

(** {2 References and Labels} *)

(** Example (using the Melt pre-processor):

[let lbl_intro = label ()]

[let intro = section ~label: lbl_intro "This is Sect.~{ref_ lbl_intro}."] *)

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

(** {2 Printing} *)

(** All printing functions take the expected mode as a parameter
(default is text). The printed expression will be coerced if its mode differs. *)

val to_buffer: ?mode: mode -> Buffer.t -> t -> unit
val to_channel: ?mode: mode -> out_channel -> t -> unit
val to_file: ?mode: mode -> string -> t -> unit
val to_string: ?mode: mode -> t -> string

(** {2 LaTeX Pervasives} *)

type documentclass = [ `Article | `Report | `Book | `Letter | `Slides | `Beamer ]
type documentoptions = [ `Landscape | `A4paper ]

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

val usepackage: ?opt: t -> t -> t
val input: t -> t
val newcommand: int -> t -> t -> t
(** [newcommand parameter_count name body] defines a new command with
 * [parameter_count] arguments, where you can use the [i]th argument by writing
 * [#i] in the body, just as in Latex. *)
val renewcommand: int -> t -> t -> t
(** Same as [newcommand] except that it can redefine existing LaTeX commands. *)

val document: ?documentclass: documentclass -> ?options: documentoptions list ->
  ?title: t -> ?author: t -> ?date: t -> ?prelude: t ->
  ?packages: (t * t) list -> t -> t
(** The [~packages] argument takes a list of [(name, opt)] where [name] is the
name of the package and [opt] is its option. This is equivalent to
using several calls to [usepackage] in the [~prelude]. *)

val block: t -> t
  (** [block x] produces [{x}]. Should only be used in some rare cases when
you want to be very precise about what LaTeX should do. *)
val index: t -> t -> t (** [index x y] produces [{x}_{y}] *)
val exponent: t -> t -> t (** [index x y] produces [{x}^{y}] *)

val tableofcontents: t
val listoffigures: t
val listoftables: t

val section: ?label: label -> t -> t
val subsection: ?label: label -> t -> t
val subsubsection: ?label: label -> t -> t
val paragraph: t -> t
val par: t
val displaymath: t -> t
val hfill: t
val footnote: t -> t
val latex_of_int: int -> t
val itemize: t list -> t
val enumerate: t list -> t
val newline: t
val newline_size: size -> t
val newpage: t
val noindent: t
val quad: t
val includegraphics: t -> t
val symbol: int -> t
val symbolc: char -> t
  (** Convert a [char] into an [int] and apply [symbol]. *)

type float_position = [ `H | `T | `P | `B ]
val float_all : float_position list

val figure: ?label: label -> ?pos: float_position list -> ?center: bool ->
  ?caption: t -> t -> t

val center: t -> t

val minipage: size -> t -> t

val emph: t -> t
val texttt: t -> t
val textsc: t -> t
val textit: t -> t
val textbf: t -> t
val mathit: t -> t
val mathbf: t -> t
val mathcal: t -> t
val tiny: t -> t

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

val parbox: size -> t -> t
  (** A box in which new lines and paragraphs may be used. Useful to display
      code listings, for instance. *)

type array_column = [ `L | `C | `R ]
type array_line

val array: array_column list -> array_line list -> t
val array_line: ?sep: size -> t list -> array_line

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
val ge: t (** greater or equal *)
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
val leadsto: t (** latexsym package *)

(** {4 Symbols to be Sorted (Stay Tuned)} *)

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

val cdots: t

val emptyset: t

type delimiter = [ `None | `Brace | `Paren | `Vert | `Bracket ]

val left: delimiter -> t
val right: delimiter -> t

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

(** {3 Slide Document Class} *)

val slide: t -> t

(** {3 Beamer Document Class} *)

module Beamer: sig
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
end

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
    string -> t
    (** Pseudocode parsing.
        @param trim apply this function first (default is [trim ['\n']])
        @param id_regexp the regular expression used to parse identifiers,
including keywords (default is words starting with a letter
followed by any number of letter or digit, followed by any number
of groups of underscore followed by at least one letter or digit:
[Str.regexp "[a-zA-Z][a-zA-Z0-9]*\\(_[a-zA-Z0-9]+\\)*"])
        @param kw_apply applied to keywords (default is [textbf])
        @param id_apply applied to identifiers (default is [mathit])
        @param rem_apply applied to remaining parts (default is [verbatim])
        @param keywords keyword list 
        @param symbols symbol list and the way they are printed
        @param keyword_symbols keyword list that should be printed in a special
way, as symbols, but parsed as identifiers

Keywords, keyword symbols and identifiers are split using underscore (['_'])
as delimiter. The first part is replaced by the corresponding [Latex.t].
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
