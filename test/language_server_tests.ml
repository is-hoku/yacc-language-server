(*open Language_server.Import*)
open Controller.Io
open Lwt.Syntax

let request ?(params = None) ~id ~method_ () =
  match params with
  | None ->
      Jsonrpc.Packet.t_of_yojson
        (Jsonrpc.Request.yojson_of_t (Jsonrpc.Request.create ~id ~method_ ()))
  | Some params ->
      Jsonrpc.Packet.t_of_yojson
        (Jsonrpc.Request.yojson_of_t
           (Jsonrpc.Request.create ~id ~method_ ~params ()))

let notification ?(params = None) ~method_ () =
  match params with
  | None ->
      Jsonrpc.Packet.t_of_yojson
        (Jsonrpc.Notification.yojson_of_t
           (Jsonrpc.Notification.create ~method_ ()))
  | Some params ->
      Jsonrpc.Packet.t_of_yojson
        (Jsonrpc.Notification.yojson_of_t
           (Jsonrpc.Notification.create ~method_ ~params ()))

let send_request wakener (input, output) =
  let* () =
    IO.write output
      (request
         ~id:(Jsonrpc.Id.t_of_yojson (`Int 0))
         ~method_:"initialize"
         ~params:
           (Some
              (`Assoc
                [
                  ("processId", `Null);
                  ("rootUri", `Null);
                  ("capabilities", `Assoc []);
                ]))
         ())
  in
  let* resp = IO.read input in
  let _ =
    match resp with
    | None -> print_endline "response for initialize request is none"
    | Some packet ->
        print_endline
          (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet))
  in
  let* () = IO.write output (notification ~method_:"initialized" ()) in
  let* () =
    IO.write output
      (request ~id:(Jsonrpc.Id.t_of_yojson (`Int 1)) ~method_:"shutdown" ())
  in
  let* resp = IO.read input in
  let _ =
    match resp with
    | None -> print_endline "response for shutdown request is none"
    | Some packet ->
        print_endline
          (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet))
  in
  let* () = IO.write output (notification ~method_:"exit" ()) in
  let* () = Lwt_io.close output in
  let* () = Lwt_io.close input in
  Lwt.wakeup wakener ();
  Lwt.return_unit

let client () =
  let sock_path = "mock_socket" in
  let sock_addr = Lwt_unix.ADDR_UNIX sock_path in
  let waiter, wakener = Lwt.wait () in
  let* _server =
    Lwt_io.establish_server_with_client_address sock_addr
      (fun _sock_addr (input, output) -> send_request wakener (input, output))
  in
  waiter

let server () =
  let socket_path = "mock_socket" in
  let module Server =
    Controller.Rpc.Make (Interactor.Lsp.Make (Datastore.Document_store.Make)) in
  let sock_addr = Lwt_unix.ADDR_UNIX socket_path in
  Lwt_io.with_connection sock_addr Server.start

let () =
  Lwt_main.run
    (Lwt.join
       [
         client ();
         (* start the server after the client has started *)
         (let* () = Lwt_unix.sleep 0.001 in
          server ());
       ])
