type verb_item =
  | VString of string
  | VOther of item

and item =
  | String of string
  | Code of item list
  | Math of item list
  | Text of item list
  | Verb of string * verb_item list
  | Par of int (* the int is the number of new lines in the source code *)

type verb_interp =
  | VIString of string
  | VIOther of interp

and interp =
  | IString of string
  | ICode of string
  | IConcat of interp list
  | IConcatCode of interp list
  | IApply of string * interp
  | IVerb of string * verb_interp list
