/**************************************************************************/
/* Copyright (c) 2009, Romain BARDOU                                      */
/* All rights reserved.                                                   */
/*                                                                        */
/* Redistribution and  use in  source and binary  forms, with  or without */
/* modification, are permitted provided that the following conditions are */
/* met:                                                                   */
/*                                                                        */
/* * Redistributions  of  source code  must  retain  the above  copyright */
/*   notice, this list of conditions and the following disclaimer.        */
/* * Redistributions in  binary form  must reproduce the  above copyright */
/*   notice, this list of conditions  and the following disclaimer in the */
/*   documentation and/or other materials provided with the distribution. */
/* * Neither the  name of Melt nor  the names of its  contributors may be */
/*   used  to endorse  or  promote products  derived  from this  software */
/*   without specific prior written permission.                           */
/*                                                                        */
/* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS */
/* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT */
/* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR */
/* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT */
/* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, */
/* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT */
/* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, */
/* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY */
/* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT */
/* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE */
/* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   */
/**************************************************************************/

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
%token <string> COMMENT
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
| COMMENT
    { Comment $1 }
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
| COMMENT
    { Comment $1 }
;

math:
| TEXT_BEGIN text_star TEXT_END
    { Text $2 }
| CODE_BEGIN code_star CODE_END
    { Code $2 }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
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
