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

{
  open Parser
  open Lexing



  type mode =
    | C
    | M
    | T
    | V of string option

  exception Lexical_error of
    (Lexing.position * Lexing.position)
      (* offending position *)
    * (mode * (Lexing.position * Lexing.position)) list
      (* stack of open modes *)
    * string
      (* further explanation *)

  let loc lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  let lex_error lexbuf stack s =
    Printf.ksprintf (fun s -> raise (Lexical_error(loc lexbuf, stack, s))) s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let get_stack,get_mode, begin_mode, end_mode, reset_mode, top_level =
    let mode = ref [] in
    (* get stack is for use in [lex_error] *)
    begin fun () -> !mode end,
    begin fun () ->
      match !mode with
        | (m,_)::_ -> m
        | [] -> C
    end,
    begin fun m lexbuf ->
      mode := (m , loc lexbuf) :: !mode;
      match m with
        | C -> CODE_BEGIN
        | M -> MATH_BEGIN
        | T -> TEXT_BEGIN
        | V apply -> VERB_BEGIN apply
    end,
    begin fun lexbuf ->
      match !mode with
        | (m,_)::rem ->
            mode := rem;
            begin match m with
              | C -> CODE_END
              | M -> MATH_END
              | T -> TEXT_END
              | V _ -> VERB_END
            end
        | [] -> lex_error lexbuf !mode "mismatched mode delimiter"
    end,
    begin fun () ->
      mode := []
    end,
    begin fun () ->
      !mode = []
    end

  (* should be defined inside the tuple, but we're facing the value restriction
      here, and it so happens that [lex_error] must have arbitrary return type,
      thus the convoluted workaround. *)
  let lex_error lexbuf s = lex_error lexbuf (get_stack ()) s

  let verb_buf = Buffer.create 16

  let comment_buf = Buffer.create 42
  let comment_nests = ref 0

  let start_comment () =
    incr comment_nests;
    Buffer.add_string comment_buf "(*"

  (* Close the current comment. If we are still in a comment, raise Exit.
     Else, return a COMMENT token containing the whole comment. *)
  let end_comment () =
    decr comment_nests;
    Buffer.add_string comment_buf "*)";
    if !comment_nests >= 1 then raise Exit;
    let s = Buffer.contents comment_buf in
    Buffer.reset comment_buf;
    COMMENT s

  let pragma_return lexbuf =
    newline lexbuf;
    STRING "\n" (* to keep the line count correct *)

  let verbatim_delims = Hashtbl.create 7

  let add_verb_delim lexbuf delim ident =
    match delim with
      | '$' | '"' | '{' ->
          lex_error lexbuf
            "Character '%c' is not allowed as a verbatim delimiter." delim
      | _ ->
          Hashtbl.add verbatim_delims delim ident
}

let space = [' ' '\t']
let lalpha = ['a'-'z']
let ualpha = ['A'-'Z']
let alpha = (lalpha | ualpha)
let num = ['0'-'9']
let alpha_num = (alpha | num)
let ident = alpha (alpha_num | '_')*

rule code = parse
  | "##" { pragma lexbuf }

  | '"' { begin_mode T lexbuf }
  | '$' { begin_mode M lexbuf }
  | '{' { begin_mode C lexbuf }
  | '}' { end_mode lexbuf }
  | '\n' { newline lexbuf; STRING "\n" }

  | "\\\"" { STRING "\"" }
  | "\\\\" { STRING "\\\\" }
  | "\\r" { STRING "\\r" }
  | "\\n" { STRING "\\n" }
  | "\\t" { STRING "\\t" }
  | '\\' num num num as x { STRING x }
  | '\\' [^ '"' '\\' 'r' 'n' 't' '0'-'9']
  | '\\' num [^'0'-'9']
      { lex_error lexbuf "invalid escaping in code mode" }

  | "(*" { start_comment (); comment lexbuf }
  | '(' { STRING "(" }
  | '#' { STRING "#" }

  | [^ '"' '$' '}' '{' '\n' '\\' '(' '#']+ { STRING(lexeme lexbuf) }
  | eof
      { if top_level () then EOF else
          lex_error lexbuf "unexpected end of file in code mode" }

and pragma = parse
  | "plugin" { pragma_plugin lexbuf; pragma_return lexbuf }
  | "verbatim" { pragma_verbatim lexbuf; pragma_return lexbuf }
  | _ { lex_error lexbuf "syntax error in pragma" }

and pragma_plugin = parse
  | space+ (ident as name) space* '\n'
      { Plugin_private.load_plugin name }
  | _ { lex_error lexbuf "syntax error in pragma plugin" }

and pragma_verbatim = parse
  | space* '\'' (_ as delim) '\'' space* '=' space*
    ((ident ('.' ident)*) as ident) space* '\n'
      { add_verb_delim lexbuf delim ident }
  | _ { lex_error lexbuf "syntax error in pragma verbatim" }

and comment = parse
  | "*)" { try end_comment () with Exit -> comment lexbuf }
  | "(*" { start_comment (); comment lexbuf }
  | '\n' { newline lexbuf; Buffer.add_char comment_buf '\n'; comment lexbuf }
  | "\\\"" { Buffer.add_char comment_buf '"'; comment lexbuf }
  | (_ as c) { Buffer.add_char comment_buf c; comment lexbuf }
  | eof { lex_error lexbuf "unexpected end of file in comment" }

and math = parse
  | '"' { begin_mode T lexbuf }
  | '{' { begin_mode C lexbuf }
  | '}' { lex_error lexbuf "end of Code mode while in Math mode"}
  | '$' { end_mode lexbuf }
  | '\n' { newline lexbuf; STRING "\n" }

  | '%' { STRING "\\%" }

  | "\\\\" { STRING "\\\\" }
  | "\\{" { STRING "\\{" }
  | "\\}" { STRING "\\}" }
  | "\\$" { STRING "\\$" }
  | "\\\"" { STRING "\"" }
  | "\\&" { STRING "\\&" }
  | "\\ " { STRING "\\ " }
  | "\\_" { STRING "\\_" }

  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ' '_']
      { lex_error lexbuf "invalid escaping in math mode" }

  | "(*" { start_comment (); comment lexbuf }
  | '(' { STRING "(" }

  | [^ '"' '$' '{' '\n' '\\' '}' '%' '(']+ { STRING(lexeme lexbuf) }
  | eof { lex_error lexbuf "unexpected end of file in math mode" }

and text = parse
  | '$' { begin_mode M lexbuf }
  | '{' { begin_mode C lexbuf }
  | '}' { lex_error lexbuf "end of Code mode while in Text mode"}
  | '"' { end_mode lexbuf }
  | "<:" (['a'-'z' 'A'-'Z' '0'-'9' '.' ' ' '_']+ as apply) ':'
      { begin_mode (V(Some apply)) lexbuf }
  | '<'
      { begin_mode (V None) lexbuf }
  | ('\n' (' ' | '\t' )* )+ '\n'
      { let s = lexeme lexbuf in
	let l = ref 0 in
	String.iter (fun c -> if c='\n' then (newline lexbuf ; incr l)) s;
        PAR !l }
  | '\n' 
      { newline lexbuf; STRING "\n" }
  | '#' { STRING "\\#" }
  | '_' { STRING "\\_" }
  | '%' { STRING "\\%" }

  | "\\\\" { STRING "\\\\" }
  | "\\{" { STRING "\\{" }
  | "\\}" { STRING "\\}" }
  | "\\$" { STRING "\\$" }
  | "\\\"" { STRING "\"" }
  | "\\&" { STRING "\\&" }
  | "\\ " { STRING "\\ " }
  | "\\'" { STRING "\\'" }
  | "\\`" { STRING "\\`" }

  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
      { lex_error lexbuf "invalid escaping in text mode" }

  | "(*" { start_comment (); comment lexbuf }
  | '(' { STRING "(" }

  | [^ '"' '$' '{' '<' '\n' '\\' '#' '_' '^' '}' '%' '(']+
      { STRING(lexeme lexbuf) }
  | eof { lex_error lexbuf "unexpected end of file in text mode" }

and verb = parse
  | '>' { end_mode lexbuf }
  | '"' { begin_mode T lexbuf }
  | '$' { begin_mode M lexbuf }
  | '{' { begin_mode C lexbuf }
  | '<' { verb_item '>' lexbuf }
  | (_ as c) { verb_item c lexbuf }
  | eof { lex_error lexbuf "unexpected end of file in verbatim mode" }

and verb_item delim = parse
  | (_ as c)
      { if c = delim then begin
          let s = Buffer.contents verb_buf in
          Buffer.reset verb_buf;
          VERB_ITEM(delim, s)
        end else begin
          Buffer.add_char verb_buf c;
          verb_item delim lexbuf
        end }
  | eof { lex_error lexbuf "unexpected end of file in verbatim mode" }

{
  let token lexbuf =
    match get_mode () with
      | C -> code lexbuf
      | M -> math lexbuf
      | T -> text lexbuf
      | V _ -> verb lexbuf
}
