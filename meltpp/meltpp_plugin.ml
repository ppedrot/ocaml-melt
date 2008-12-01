open Format

type verbatim_item = Plugin_private.verbatim_item
type verbatim_function = Plugin_private.verbatim_function

let declare_verbatim_function =
  Hashtbl.add Plugin_private.verbatim_functions

let rec list_insert acc x = function
  | a::r -> list_insert (a::x::acc) x r
  | [] -> match List.rev acc with
      | [] -> []
      | _::r -> r
let list_insert x = list_insert [] x

let list_iter_concat fmt f = function
  | [] ->
      fprintf fmt "(text \"\")"
  | l ->
      let l = List.map (fun x -> `I x) l in
      let l = list_insert `C l in
      fprintf fmt "(";
      List.iter begin function
        | `I x -> fprintf fmt "(%a)" f x
        | `C -> fprintf fmt "^^"
      end l;
      fprintf fmt ")"

let escape_except_newline s =
  let s = String.escaped s in
  Str.global_replace (Str.regexp_string "\\n") "\n" s

let verbatim_complex name: verbatim_function = fun f l ->
  fprintf f "(%s [" name;
  List.iter begin function
    | `V s -> fprintf f "`V \"%s\"" (escape_except_newline s)
    | `A a -> fprintf f "`A(%a)" a ()
  end l;
  fprintf f "])"

let verbatim_simple name: verbatim_function = fun f l ->
  let l = list_insert `C (l :> [ verbatim_item | `C ] list) in
  fprintf f "(";
  List.iter begin function
    | `V s -> fprintf f "(%s \"%s\")" name (escape_except_newline s)
    | `A a -> fprintf f "(%a)" a ()
    | `C -> fprintf f "^^"
  end l;
  fprintf f ")"
