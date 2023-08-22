open Import
open Io
open Lwt.Syntax

let send_request _ (input, output) =
  print_endline "start requests";
  let* () = Lwt_unix.sleep 1. in
  let* () =
    print_endline "send first request";
    IO.write output
      (Jsonrpc.Packet.t_of_yojson
         (Jsonrpc.Request.yojson_of_t
            (Jsonrpc.Request.create
               ~id:(Jsonrpc.Id.t_of_yojson (`Int 0))
               ~method_:"initialize" ())))
  in
  let* () = Lwt_unix.sleep 2. in
  let* () =
    print_endline "send second request";
    IO.write output
      (Jsonrpc.Packet.t_of_yojson
         (Jsonrpc.Request.yojson_of_t
            (Jsonrpc.Request.create
               ~id:(Jsonrpc.Id.t_of_yojson (`Int 0))
               ~method_:"initialize" ())))
  in
  let* () = Lwt_unix.sleep 3. in
  let* () =
    print_endline "send third request";
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
  print_endline
    ("Mock Client is Running at "
    ^ Unix.string_of_inet_addr Unix.inet_addr_loopback);
  Lwt_io.shutdown_server server
