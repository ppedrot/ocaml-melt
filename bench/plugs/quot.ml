open Meltpp_plugin
open Format

let test fmt l =
  list_iter_concat fmt begin fun fmt -> function
    | `V s -> fprintf fmt "text \"--Quotation: %s--\"" s
    | `A a -> fprintf fmt "text \"--Anti-quotation: \"^^%a^^text \"--\"" a ()
  end l

let () = declare_verbatim_function "test" test
