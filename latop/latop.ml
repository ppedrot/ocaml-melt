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
      eprintf "%s\n" line
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
