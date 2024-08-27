(* TEST
 modules = "test_c_thread_has_lock_cstubs.c";
 runtime5;
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

external test_with_lock : unit -> bool = "with_lock"
external test_without_lock : unit -> bool = "without_lock"

let passed b = Printf.printf (if b then "passed\n" else "failed\n")

let f () =
  (* CR ocaml 5 domains: all systhreads will always have [caml_state != NULL] *)
  passed (test_without_lock ());
  passed (test_with_lock ())

let _ =
  f ();
  let t = Thread.create f () in
  Thread.join t
