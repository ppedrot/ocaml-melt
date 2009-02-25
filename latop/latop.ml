open Format
open Str

let fle = regexp "\\([^:]*\\):\\([0-9]+\\): *\\(.*\\)"
let warning = regexp "LaTeX Warning: *\\(.*\\)"
let fatal = regexp " *==> Fatal error"
let ucs = regexp " *Undefined control sequence"
let ucs_cmd = regexp ".*\\\\\\([^\\]+\\)"
let badness = regexp ".*badness"

type error =
  | Fatal
  | Undefined_control_sequence of string
  | Unknown of string

type line =
  | Normal of string
  | Error of string * int * error (** file, line, error *)
  | Warning of string

let parse_error err =
  if string_match fatal err 0 then
    Fatal
  else if string_match ucs err 0 then
    let line = read_line () in
    ignore (read_line ());
    if string_match ucs_cmd line 0 then
      Undefined_control_sequence (matched_group 1 line)
    else
      Undefined_control_sequence "???"
  else
    Unknown err

let parse_line line =
  if string_match fle line 0 then
    let file = matched_group 1 line in
    let line_nb = int_of_string (matched_group 2 line) in
    let err = parse_error (matched_group 3 line) in
    Error (file, line_nb, err)
  else if string_match warning line 0 then
    Warning (matched_group 1 line)
  else if string_match badness line 0 then
    Warning line
  else
    Normal line

let reprint_line = function
  | Normal line ->
      printf "%s\n" line
  | Error (file, line, error) ->
      if error <> Fatal then
        eprintf "File \"%s\", line %d:\n" file line;
      begin match error with
        | Fatal ->
            printf "%!";
            eprintf "%!";
            exit 1
        | Undefined_control_sequence cmd ->
            eprintf "Undefined control sequence: %s\n" cmd
        | Unknown msg ->
            eprintf "%s\n" msg
      end
  | Warning msg ->
      eprintf "Latex Warning: %s\n" msg

let () =
  printf "latop !\n%!";
  try
    while true do
      reprint_line (parse_line (read_line ()))
    done
  with End_of_file ->
    printf "%!";
    eprintf "%!";
