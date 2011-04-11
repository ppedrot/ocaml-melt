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

let files = Queue.create ()
let maxlen = ref 0

let speclist = []
let anon_fun x =
  maxlen := max !maxlen (String.length x);
  Queue.add x files
let usage_msg = "ocaml bench.ml <files>"

let () =
  Arg.parse speclist anon_fun usage_msg;
  if Queue.is_empty files then begin
    Arg.usage speclist usage_msg;
    exit 0
  end

let file opt f =
  if not (Sys.file_exists f) then begin
    eprintf "Error: File not found: %s\n" f;
    exit 2
  end;
  let cwd = Sys.getcwd () in
  let log = sprintf "%s/%s.bench.log" cwd (Filename.basename f) in
  if Sys.file_exists log then Sys.remove log;
  let cmd = sprintf
    "%s/../_build/melt/tool.byte -P %s/../_build/bench/plugs \
-I %s/../_build/latex -I %s/../_build/melt \
-latop %s/../_build/latop/latop.byte \
-meltpp %s/../_build/meltpp/main.byte \
%s \
%s 2>> %s >> %s" cwd cwd cwd cwd cwd cwd opt f log log in
  let dots = String.make (!maxlen - String.length f + 5) '.' in
  match Sys.command cmd with
    | 0 ->
        printf "%s %s: OK\n%!" f dots;
        Sys.remove log
    | n ->
        printf "%s %s: FAILED (code %d)\n%!" f dots n

let () =
  printf "\nTesting melt examples by compiling to .ps...\n%!";
  Queue.iter (file "-ps") files;
  printf "\nTesting melt examples by compiling to .pdf...\n%!";
  Queue.iter (file "-pdf") files;
