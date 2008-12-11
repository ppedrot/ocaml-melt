open Meltpp_plugin
open Format

let test fmt l =
  list_iter_concat fmt begin fun fmt -> function
    | `V s -> fprintf fmt "text \"--Quotation V: %s--\"" s
    | `C a -> fprintf fmt "text \"--Anti-quotation C: \"^^%a^^text \"--\"" a ()
    | `M a -> fprintf fmt "text \"--Anti-quotation M: \"^^%a^^text \"--\"" a ()
    | `T a -> fprintf fmt "text \"--Anti-quotation T: \"^^%a^^text \"--\"" a ()
  end l

let () = declare_verbatim_function "test" test
