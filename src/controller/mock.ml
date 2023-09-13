open Language_server.Import
open Io
open Lwt.Syntax

let send_request wakener (input, output) =
  let* () =
    IO.write output
      (Jsonrpc.Packet.t_of_yojson
         (Jsonrpc.Request.yojson_of_t
            (Jsonrpc.Request.create
               ~id:(Jsonrpc.Id.t_of_yojson (`Int 0))
               ~method_:"initialize" ())))
  in
  let* () = Lwt_unix.sleep 2. in
  let* () =
    IO.write output
      (Jsonrpc.Packet.t_of_yojson
         (Jsonrpc.Notification.yojson_of_t
            (Jsonrpc.Notification.create ~params:(`Assoc [])
               ~method_:"initialized" ())))
  in
  let* () = Lwt_unix.sleep 3. in
  let* () =
    IO.write output
      (Jsonrpc.Packet.t_of_yojson
         (Jsonrpc.Notification.yojson_of_t
            (Lsp.Server_notification.to_jsonrpc
               (Lsp.Server_notification.ShowMessage
                  (ShowMessageParams.create
                     ~message:"Hello, this is language server!"
                     ~type_:MessageType.Info)))))
  in
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
