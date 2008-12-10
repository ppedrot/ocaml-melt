{
  open Parser
  open Lexing

  exception Lexical_error of (Lexing.position * Lexing.position) * string

  let loc lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  let lex_error lexbuf s =
    raise (Lexical_error(loc lexbuf, s))

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  type mode =
    | C
    | M
    | T
    | V of string option

  let get_mode, begin_mode, end_mode, reset_mode, top_level =
    let mode = ref [C] in
    begin fun () ->
      match !mode with
        | m::_ -> m
        | [] -> assert false
    end,
    begin fun m ->
      mode := m :: !mode;
      match m with
        | C -> CODE_BEGIN
        | M -> MATH_BEGIN
        | T -> TEXT_BEGIN
        | V apply -> VERB_BEGIN apply
    end,
    begin fun lexbuf ->
      match !mode with
        | m::((_::_) as rem) ->
            mode := rem;
            begin match m with
              | C -> CODE_END
              | M -> MATH_END
              | T -> TEXT_END
              | V _ -> VERB_END
            end
        | [_] -> lex_error lexbuf "mismatched mode delimiter"
        | [] -> assert false
    end,
    begin fun () ->
      mode := [C]
    end,
    begin fun () ->
      !mode = [C]
    end

  let verb_buf = Buffer.create 16

  let comment_buf = Buffer.create 42
  let comment_nests = ref 0

  let start_comment () =
    incr comment_nests;
    Buffer.add_string comment_buf "(*"

  (* Close the current comment. If we are still in a comment, raise Exit.
     Else, return a STRING token containing the whole comment. *)
  let end_comment () =
    decr comment_nests;
    Buffer.add_string comment_buf "*)";
    if !comment_nests >= 1 then raise Exit;
    let s = Buffer.contents comment_buf in
    Buffer.reset comment_buf;
    STRING s

  let pragma_return lexbuf =
    newline lexbuf;
    STRING "\n" (* to keep the line count correct *)

  let verbatim_delims = Hashtbl.create 7
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

  | '"' { begin_mode T }
  | '$' { begin_mode M }
  | '{' { begin_mode C }
  | '}' { end_mode lexbuf }
  | '\n' { newline lexbuf; STRING "\n" }

  | "\\\"" { STRING "\"" }
  | "\\\\" { STRING "\\\\" }
  | "\\r" { STRING "\\r" }
  | "\\n" { STRING "\\n" }
  | "\\t" { STRING "\\t" }
  | '\\' [^ '"' '\\' 'r' 'n' 't']
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
  | space* '\'' (_ as delim) '\'' space* '=' space* (ident as ident) space* '\n'
      { Hashtbl.add verbatim_delims delim ident }
  | _ { lex_error lexbuf "syntax error in pragma verbatim" }

and comment = parse
  | "*)" { try end_comment () with Exit -> comment lexbuf }
  | "(*" { start_comment (); comment lexbuf }
  | '\n' { newline lexbuf; Buffer.add_char comment_buf '\n'; comment lexbuf }
  | (_ as c) { Buffer.add_char comment_buf c; comment lexbuf }
  | eof { lex_error lexbuf "unexpected end of file in comment" }

and math = parse
  | '"' { begin_mode T }
  | '{' { begin_mode C }
  | '$' { end_mode lexbuf }
  | '\n' { newline lexbuf; STRING "\n" }

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

  | [^ '"' '$' '{' '\n' '\\']+ { STRING(lexeme lexbuf) }
  | eof { lex_error lexbuf "unexpected end of file in math mode" }

and text = parse
  | '$' { begin_mode M }
  | '{' { begin_mode C }
  | '"' { end_mode lexbuf }
  | "<:" (['a'-'z' 'A'-'Z' '0'-'9' '.' ' ']+ as apply) ':'
      { begin_mode (V(Some apply)) }
  | '<'
      { begin_mode (V None) }
  | '\n'+
      { let s = lexeme lexbuf in
        let l = String.length s in
        for i = 1 to l do
          newline lexbuf
        done;
        if l > 1 then PAR l else STRING "\n" }

  | '#' { STRING "\\#" }
  | '_' { STRING "\\_" }

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

  | [^ '"' '$' '{' '<' '\n' '\\' '#' '_' '^']+ { STRING(lexeme lexbuf) }
  | eof { lex_error lexbuf "unexpected end of file in text mode" }

and verb = parse
  | '>' { end_mode lexbuf }
  | '"' { begin_mode T }
  | '$' { begin_mode M }
  | '{' { begin_mode C }
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
