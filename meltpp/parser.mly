%{
  open Ast

  let verb name items =
    let name = match name with
      | Some name -> VNUser name
      | None ->
          match items with
            | `Item(delim, _) :: _ -> VNDelim delim
            | _ -> VNDefault
    in
    let items = List.map begin function
      | `Item(_, s) -> VString s
      | `Code x -> VCode x
      | `Math x -> VMath x
      | `Text x -> VText x
    end items in
    Verb(name, items)
%}

%token <string> STRING
%token EOF
%token TEXT_BEGIN TEXT_END
%token MATH_BEGIN MATH_END
%token CODE_BEGIN CODE_END
%token <string option> VERB_BEGIN
%token VERB_END
%token <char * string> VERB_ITEM
%token <int> PAR

%type <Ast.item> file
%start file

%%

file:
| code_star EOF
    { Code $1 }
;

code_star:
| code code_star
    { $1::$2 }
|
    { [] }
;

text_star:
| text text_star
    { $1::$2 }
|
    { [] }
;

math_star:
| math math_star
    { $1::$2 }
|
    { [] }
;

verb_star:
| verb verb_star
    { $1::$2 }
|
    { [] }
;

code:
| TEXT_BEGIN text_star TEXT_END
    { Text $2 }
| MATH_BEGIN math_star MATH_END
    { Math $2 }
| CODE_BEGIN code_star CODE_END
    { Code [String "{"; Code $2; String "}"] }
| STRING
    { String $1 }
;

text:
| MATH_BEGIN math_star MATH_END
    { Math $2 }
| CODE_BEGIN code_star CODE_END
    { Code $2 }
| VERB_BEGIN verb_star VERB_END
    { verb $1 $2 }
| STRING
    { String $1 }
| PAR
    { Par $1 }
;

math:
| TEXT_BEGIN text_star TEXT_END
    { Text $2 }
| CODE_BEGIN code_star CODE_END
    { Code $2 }
| STRING
    { String $1 }
;

verb:
| TEXT_BEGIN text_star TEXT_END
    { `Text $2 }
| MATH_BEGIN math_star MATH_END
    { `Math $2 }
| CODE_BEGIN code_star CODE_END
    { `Code $2 }
| VERB_ITEM
    { `Item $1 }
;
