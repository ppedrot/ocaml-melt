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

open Printf

let includes = ref []

type verbatim_item = [
| `V of string
| `C of Format.formatter -> unit -> unit
| `M of Format.formatter -> unit -> unit
| `T of Format.formatter -> unit -> unit
]

type verbatim_function = Format.formatter -> verbatim_item list -> unit

let verbatim_functions: (string, verbatim_function) Hashtbl.t = Hashtbl.create 7

let find name =
  let rec f = function
    | [] ->
        eprintf "Error: Cannot find plugin %s.\n" name;
        exit 2
    | i::r ->
        let name = Filename.concat i name in
	if Sys.file_exists  (name^".cmxs") then name^".cmxs" else
          if Sys.file_exists (name^".cmo") then name^".cmo" else
            if Sys.file_exists (name^".cma") then name^".cma" else
              f r
  in f !includes

let load_plugin =
  let init = ref false in
  fun name ->
    if not !init then begin
      Dynlink.init ();
      Dynlink.prohibit ["Ast"; "Lexer"; "Parser"; "Plugin_private"];
      init := true;
    end;
    try
      Dynlink.loadfile (find name)
    with Sys_error s ->
      eprintf "%s\nError: Cannot load plugin %s.\n" s name;
      exit 2
