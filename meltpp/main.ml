open Ast
open Lexing
open Lexer
open Parser
open Format
open Meltpp_plugin

let dir = ref ""

let meltpp_verbatim_default_name = "meltpp_verbatim_default"

let locate_error (b, e) x =
  let buf = Buffer.create 42 in
  kfprintf
    (fun buffmt ->
       fprintf buffmt "@?";
       fprintf std_formatter
         "File \"%s%s\", line %d, characters %d-%d:@\n%s@."
         !dir
         b.pos_fname b.pos_lnum (b.pos_cnum - b.pos_bol)
         (e.pos_cnum - b.pos_bol) (Buffer.contents buf);
       exit 1)
    (formatter_of_buffer buf) x

let parse_file f =
  let ic = open_in f in
  let l = from_channel ic in
  l.lex_curr_p <- { l.lex_curr_p with pos_fname = f };
  try
    file token l
  with Lexer.Lexical_error(loc, s) ->
    locate_error loc "Parse error: %s" s

let rec interp code = function
  | String s ->
      if code then ICode s else IString s
  | Code l ->
      interp_list true l
  | Math l ->
      IApply("mode M", interp_list false l)
  | Text l ->
      IApply("mode T", interp_list false l)
  | Verb(f, l) ->
      let f = match f with
        | VNDefault -> meltpp_verbatim_default_name
        | VNUser s -> s
        | VNDelim c -> Hashtbl.find Lexer.verbatim_delims c
      in
      let l = List.map begin function
        | VString s -> VIString s
        | VCode l -> VICode(interp_list true l)
        | VMath l -> VIMath(interp_list false l)
        | VText l -> VIText(interp_list false l)
      end l in
      IVerb(f, l)
  | Par n ->
      ICode ("(par)" ^ String.make n '\n')

and interp_list code l =
  let l = List.map (interp code) l in
  if code then IConcatCode l else IConcat l

let rec print f = function
  | ICode s ->
      fprintf f "%s" s
  | IString "\n" ->
      fprintf f "(text \"\\n\")\n"
  | IString s ->
      fprintf f "(text \"%s\")" (String.escaped s)
  | IConcat l -> 
      begin match l with
        | [] ->
            fprintf f "(text \"\")"
        | [x] ->
            print f x
        | x::rem ->
            fprintf f "(";
            print f x;
            List.iter (fun y -> fprintf f " ^^ (%a)" print y) rem;
            fprintf f ")"
      end
  | IConcatCode l ->
      List.iter (print f) l
  | IApply(n, i) ->
      fprintf f "(%s (%a))" n print i
  | IVerb(vf, l) ->
      let vf = try
        Hashtbl.find Plugin_private.verbatim_functions vf
      with Not_found ->
        verbatim_complex vf
      in
      let l = List.map begin function
        | VIString s -> `V s
        | VICode i -> `C (fun f () -> print f i)
        | VIMath i -> `M (fun f () -> print f i)
        | VIText i -> `T (fun f () -> print f i)
      end l in
      vf f l

let output = ref ""
let files = Queue.create ()
let plugins = Queue.create ()
let opens = Queue.create ()
let add_open x = Queue.add x opens

let spec = Arg.align [
  "-o", Arg.Set_string output, " Output file name (cannot be used when \
compiling multiple files at the same time)";
  "-open", Arg.String add_open, "<Module> Add \"open Module;;\" at \
the beginning of the file";
  "-dir", Arg.Set_string dir, "<dir> Add <dir> to the file location (for \
error message locations only)";
]

let usage =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] [plugins] files
Plugins should be OCaml compiled modules or archives."

let file_ext s =
  try
    let i = String.rindex s '.' in
    String.sub s i (String.length s - i)
  with Not_found ->
    ""

let anon s =
  match file_ext s with
    | ".cma" | ".cmxa" | ".cmo" | ".cmx" -> Queue.add s plugins
    | _ -> Queue.add s files

let print_opens oc =
  Queue.iter begin fun m ->
    fprintf oc "open %s;;\n\n" m
  end opens

(* default environment *)
let () =
  declare_verbatim_function meltpp_verbatim_default_name
    (verbatim_simple "Latex.Verbatim.verbatim")

(* parse arguments *)
let () =
  Arg.parse spec anon usage;
  if !output <> "" && Queue.length files > 1 then begin
    eprintf "Cannot use the -o option when compiling multiple files \
at the same time.\n%!";
    exit 1
  end

(* command line plugins *)
let () =
  Queue.iter Plugin_private.load_plugin plugins

(* main *)
let () =
  Queue.iter begin fun filename ->
    let ast = parse_file filename in
    let outputname =
      if !output <> "" then !output else Filename.chop_extension filename in
    let ast' = interp true ast in
    let oc = open_out outputname in
    let fmt = formatter_of_out_channel oc in
    print_opens fmt;
    fprintf fmt "# 1 \"%s%s\"\n" !dir (String.escaped filename);
    print fmt ast';
    close_out oc
  end files
