(* We don't use the Arg module in order not to force the user to handle
the -pdf and -name options *)

let pdf =
  let b = ref false in
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-pdf" then b := true
  done;
  !b

let rec no_extension f =
  try
    no_extension (Filename.chop_extension f)
  with Invalid_argument "Filename.chop_extension" -> f

let name =
  let name = ref (no_extension (Filename.basename Sys.argv.(0))) in
  for i = 1 to Array.length Sys.argv - 2 do
    if Sys.argv.(i) = "-name" then
      name := Sys.argv.(i+1)
  done;
  !name

let next_name =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    Printf.sprintf "%s-melt-figure%d" name !cnt

let latex l = Mlpost.Picture.tex (Latex.to_string l)

let mlpost ?(pdf = pdf) ?file f =
  let file = match file with
    | None -> next_name ()
    | Some file -> file
  in
  let ext = if pdf then ".mps" else ".1" in
  Mlpost.Metapost.emit file f;
  Latex.includegraphics (Latex.text (file ^ ext))

let emit ?(file = name ^ ".tex") x = Latex.to_file file x

module Verbatim = struct
  type latex_verbatim_function = string -> Latex.t
  type melt_verbatim_function = [ `V of string | `A of Latex.t ] list ->
    Latex.t

  let convert f l =
    Latex.concat begin List.map begin function
      | `V s -> f s
      | `A a -> a
    end l end

  let verbatim = convert Latex.Verbatim.verbatim
  let regexps x y = convert (Latex.Verbatim.regexps x y)
  let keywords ?apply x = convert (Latex.Verbatim.keywords ?apply x)
end
