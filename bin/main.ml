open Language_server.Io
open Lwt.Syntax

let main () =
  print_endline (Unix.getcwd ());
  let* input = Lwt_io.open_file ~mode:Lwt_io.Input "packet.txt" in
  let output = Lwt_io.stdout in

  let rec read_and_write () =
    let* result = IO.read input in
    match result with
    | None -> Lwt.return_unit
    | Some json ->
        let* () = IO.write output json in
        read_and_write ()
  in

  let* () = read_and_write () in
  let* () = Lwt_io.close output in
  Lwt.return_unit

let () = Lwt_main.run (main ())
