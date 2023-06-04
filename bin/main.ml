open Language_server.Io
open Lwt.Syntax

(*let read_input_and_write_to_file file_path =*)
(*let* input = Lwt_io.open_file ~mode:Lwt_io.Input file_path in*)
let main () =
  let input = Lwt_io.stdin in
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

(*let file_path = "json.txt"
  let () = Lwt_main.run (read_input_and_write_to_file file_path)*)
let () = Lwt_main.run (main ())
