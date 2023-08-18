open Io
open Lwt.Syntax

let send_request _ (input, output) =
  print_endline "handling client";
  let* input_file = Lwt_io.open_file ~mode:Lwt_io.Input "packet.txt" in

  let rec read_and_write () =
    let* result = IO.read input_file in
    match result with
    | None -> Lwt.return_unit
    | Some packet ->
        let* () = IO.write output packet in
        read_and_write ()
  in
  let* () = read_and_write () in
  let* () = Lwt_io.close output in
  let* () = Lwt_io.close input in
  Lwt.return_unit

let client () =
  let sock_path = "/run/user/1000/mock_socket" in
  let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let sock_addr = Lwt_unix.ADDR_UNIX sock_path in
  print_endline "created client socket";
  let* () = Lwt_unix.bind sock sock_addr in
  print_endline ("bind client socket to " ^ sock_path);
  Lwt_unix.listen sock 1;
  print_endline "listening client socket";
  let* server =
    Lwt_io.establish_server_with_client_address ~no_close:true sock_addr
      send_request
  in
  Lwt_io.shutdown_server server
(*
  let* () =
    Lwt_unix.connect sock
      (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, socket_port))
  in
  *)
