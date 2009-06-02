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

let file f =
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
-meltpp %s/../_build/meltpp/main.byte %s \
2>> %s >> %s" cwd cwd cwd cwd cwd cwd f log log in
  let dots = String.make (!maxlen - String.length f + 5) '.' in
  match Sys.command cmd with
    | 0 ->
        printf "%s %s: OK\n%!" f dots;
        Sys.remove log
    | n ->
        printf "%s %s: FAILED (code %d)\n%!" f dots n

let () =
  Queue.iter file files
