type verb_name =
  | VNUser of string
  | VNDelim of char
  | VNDefault

type verb_item =
  | VString of string
  | VCode of item list
  | VMath of item list
  | VText of item list

and item =
  | String of string
  | Code of item list
  | Math of item list
  | Text of item list
  | Verb of verb_name * verb_item list
  | Par of int (* the int is the number of new lines in the source code *)

type verb_interp =
  | VIString of string
  | VICode of interp
  | VIMath of interp
  | VIText of interp

and interp =
  | IString of string
  | ICode of string
  | IConcat of interp list
  | IConcatCode of interp list
  | IApply of string * interp
  | IVerb of string * verb_interp list
