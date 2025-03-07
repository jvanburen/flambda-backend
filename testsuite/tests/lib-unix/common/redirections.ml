(* TEST
 readonly_files = "reflector.ml";
 unset XVAR;
 hassysthreads;
 {
   program = "${test_build_directory}/redirections.byte";
   setup-ocamlc.byte-build-env;
   program = "${test_build_directory}/reflector.exe";
   all_modules = "reflector.ml";
   ocamlc.byte;
   include systhreads;
   program = "${test_build_directory}/redirections.byte";
   all_modules = "redirections.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   program = "${test_build_directory}/redirections.opt";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/reflector.exe";
   all_modules = "reflector.ml";
   ocamlopt.byte;
   include systhreads;
   program = "${test_build_directory}/redirections.opt";
   all_modules = "redirections.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)


let cat file =
  let fd = Unix.openfile file [Unix.O_RDONLY] 0 in
  let buf = Bytes.create 1024 in
  let rec cat () =
    let n = Unix.read fd buf 0 (Bytes.length buf) in
    if n > 0 then (ignore(Unix.write Unix.stdout buf 0 n); cat ())
  in cat (); Unix.close fd

let out fd txt =
  ignore (Unix.write_substring fd txt 0 (String.length txt))

let refl =
  Filename.concat Filename.current_dir_name "reflector.exe"

let test_createprocess systemenv =
  let f_out =
    Unix.(openfile "./tmpout.txt" [O_WRONLY;O_TRUNC;O_CREAT;O_CLOEXEC] 0o600) in
  let f_err =
    Unix.(openfile "./tmperr.txt" [O_WRONLY;O_TRUNC;O_CREAT;O_CLOEXEC] 0o600) in
  let (p_exit, p_entrance) =
    Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process_env
       refl
       [| refl; "-i2o"; "-i2e"; "-o"; "123"; "-e"; "456"; "-i2o"; "-v"; "XVAR"
       |]
       (Array.append [| "XVAR=xvar" |] systemenv)
       p_exit f_out f_err in
  out p_entrance "aaaa\n";
  out p_entrance "bbbb\n";
  Unix.close p_entrance;
  let (_, status) = Unix.waitpid [] pid in
  Unix.close p_exit; Unix.close f_out; Unix.close f_err;
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n";
  out Unix.stdout "---- File tmpout.txt\n";
  cat "./tmpout.txt";
  out Unix.stdout "---- File tmperr.txt\n";
  cat "./tmperr.txt";
  Sys.remove "./tmpout.txt";
  Sys.remove "./tmperr.txt"

let test_2ampsup1 () =    (* 2>&1 redirection, cf. GPR#1105 *)
  let pid =
    Unix.create_process
      refl
      [| refl; "-o"; "123"; "-e"; "456"; "-o"; "789" |]
      Unix.stdin Unix.stdout Unix.stdout in
  let (_, status) = Unix.waitpid [] pid in
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n"

let test_swap12 () =    (* swapping stdout and stderr *)
  (* The test harness doesn't let us check contents of stderr,
     so just output on stdout (after redirection) *)
  let pid =
    Unix.create_process
      refl
      [| refl; "-e"; "123" |]
      Unix.stdin Unix.stderr Unix.stdout in
  let (_, status) = Unix.waitpid [] pid in
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n"

let test_12tofile () =   (* > file 2>&1 *)
  let f =
    Unix.(openfile "./tmpout.txt" [O_WRONLY;O_TRUNC;O_CREAT;O_CLOEXEC] 0o600) in
  let pid =
    Unix.create_process
       refl
       [| refl; "-o"; "123"; "-e"; "456"; "-o"; "789" |]
       Unix.stdin f f in
  let (_, status) = Unix.waitpid [] pid in
  Unix.close f;
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n";
  out Unix.stdout "---- File tmpout.txt\n";
  cat "./tmpout.txt";
  Sys.remove "./tmpout.txt"

let test_open_process_in () =
  let ic = Unix.open_process_in (refl ^ " -o 123 -o 456") in
  out Unix.stdout (input_line ic ^ "\n");
  out Unix.stdout (input_line ic ^ "\n");
  let status = Unix.close_process_in ic in
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n"

let test_open_process_out () =
  let oc = Unix.open_process_out (refl ^ " -i2o -i2o -i2o") in
  output_string oc "aa\nbbbb\n"; close_out oc;
  let status = Unix.close_process_out oc in
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n"

let test_open_process_full systemenv =
  let ((o, i, e) as res) =
    Unix.open_process_full
      (refl ^ " -o 123 -i2o -e 456 -i2e -v XVAR")
      (Array.append [|"XVAR=xvar"|] systemenv) in
  output_string i "aa\nbbbb\n"; close_out i;
  for _i = 1 to 3 do
    out Unix.stdout (input_line o ^ "\n")
  done;
  for _i = 1 to 2 do
    out Unix.stdout (input_line e ^ "\n")
  done;
  let status = Unix.close_process_full res in
  if status <> Unix.WEXITED 0 then
    out Unix.stdout "!!! reflector exited with an error\n"

let _ =
  let env = Unix.environment() in
  (* The following 'close' makes things more difficult.
     Under Unix it works fine, but under Win32 create_process
     gives an error if one of the standard handles is closed. *)
  (* Unix.close Unix.stdin; *)
  out Unix.stdout "** create_process\n";
  test_createprocess env;
  out Unix.stdout "** create_process 2>&1 redirection\n";
  test_2ampsup1();
  out Unix.stdout "** create_process swap 1-2\n";
  test_swap12();
  out Unix.stdout "** create_process >file 2>&1\n";
  test_12tofile();
  out Unix.stdout "** open_process_in\n";
  test_open_process_in();
  out Unix.stdout "** open_process_out\n";
  test_open_process_out();
  out Unix.stdout "** open_process_full\n";
  test_open_process_full env
