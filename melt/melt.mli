(** Unification of [Mlpost] and [Latex]. *)

(** Convert some LaTeX into a picture. *)
val latex: Latex.t -> Mlpost.Picture.t

(** Emit a figure to use it in a LaTeX document. *)
val mlpost: ?pdf: bool -> ?file: string -> Mlpost.Command.figure -> Latex.t
  (**  The default  value  of [~pdf]  is  [true] if  the command  line
contain  [-pdf], and  [false] otherwise.  It should  be [true]  if the
figure will be  used in a PDF file, and  [false] otherwise. The [melt]
tool adds the [-pdf] option automatically if it is himself called with
the [-pdf] option.

The [~file] parameter may be used if you want to specify the file name
used  for the  figure Metapost  script. Otherwise,  a default  name is
chosen.  This default name is [base.melt.figureN.ext], where [base] is
the executable base name (can  be overriden with the [-name] option on
the  command line),  [N] is  the figure  index and  [ext] is  [mps] if
[~pdf] is [true] or [1] otherwise. *)

(** Emit a LaTeX file. *)
val emit: ?file: string -> Latex.t -> unit
  (**  This is  basically the  same than  [Latex.to_file]  except that
there is  a default file name  [base.tex] where [base]  is computed in
the same way than [base] in the [mlpost] function. *)

module Verbatim: sig
  (** Verbatim modes for the Melt pre-processor. *)

  (** These modes are the same as the ones in the [Latex.Verbatim] module,
except that they work with the Melt pre-processor. *)

  type latex_verbatim_function = string -> Latex.t
  type melt_verbatim_function = [ `V of string | `A of Latex.t ] list ->
    Latex.t

  val convert: latex_verbatim_function -> melt_verbatim_function
    (** Convert a verbatim function of the [Latex] module to a function
usable with the Melt pre-processor. The original function is applied to
each quotations; anti-quotations are left as it, and the resulting list
is concatenated. *)

  (** The following functions are converted from the [Latex.Verbatim] module. *)

  val verbatim: melt_verbatim_function
  val regexps: (Str.regexp * (string -> Latex.t)) list -> (string -> Latex.t) ->
    melt_verbatim_function
  val keywords: ?apply: (Latex.t -> Latex.t) -> string list ->
    melt_verbatim_function
end
