open Language_server.Import
open Language_server.Io
open Language_server.Mock
open Lwt.Syntax

let handle_client (input, output) =
  print_endline "handling client";

  (*
  let* () =
    IO.write output
      (Jsonrpc.Packet.t_of_yojson
         (Jsonrpc.Notification.yojson_of_t
            (Lsp.Server_notification.to_jsonrpc
               (Lsp.Server_notification.ShowMessage
                  (ShowMessageParams.create
                     ~message:"Hello, this is language server!"
                     ~type_:MessageType.Error)))))
  in
                     *)
  let rec read_and_write () =
    print_endline "read and write...";
    let* result = IO.read input in
    match result with
    | None -> Lwt.return_unit
    | Some packet ->
        let req = Jsonrpc.Packet.yojson_of_t packet in
        let id =
          match req with
          | `Assoc fields -> Json.field_exn fields "id" Json.Conv.int_of_yojson
          | _ -> Json.error "invalid json structure" req
        in
        let req_method =
          match req with
          | `Assoc fields ->
              Json.field_exn fields "method" Json.Conv.string_of_yojson
          | _ -> Json.error "invalid json structure" req
        in
        let* () =
          match req_method with
          | "initialize" ->
              IO.write output
                (Jsonrpc.Packet.t_of_yojson
                   (Jsonrpc.Response.yojson_of_t
                      (Jsonrpc.Response.ok
                         (Jsonrpc.Id.t_of_yojson (`Int id))
                         (InitializeResult.yojson_of_t
                            (InitializeResult.create
                               ~capabilities:
                                 (ServerCapabilities.create
                                    ~textDocumentSync:
                                      (`TextDocumentSyncOptions
                                        (TextDocumentSyncOptions.create
                                           ~openClose:true ~change:Incremental
                                           ()))
                                    ~codeActionProvider:(`Bool true) ())
                               ~serverInfo:
                                 (InitializeResult.create_serverInfo
                                    ~name:"yacc-lsp" ())
                               ())))))
          | "initialized" ->
              IO.write output
                (Jsonrpc.Packet.t_of_yojson
                   (Jsonrpc.Response.yojson_of_t
                      (Jsonrpc.Response.ok
                         (Jsonrpc.Id.t_of_yojson (`Int id))
                         (LogMessageParams.yojson_of_t
                            (LogMessageParams.create ~message:"initialized"
                               ~type_:MessageType.Info)))))
          | _ ->
              IO.write output
                (Jsonrpc.Packet.t_of_yojson
                   (Jsonrpc.Response.yojson_of_t
                      (Jsonrpc.Response.ok
                         (Jsonrpc.Id.t_of_yojson (`Int id))
                         (LogMessageParams.yojson_of_t
                            (LogMessageParams.create ~message:"unknown request"
                               ~type_:MessageType.Info)))))
        in
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
        read_and_write ()
  in
  let* () = read_and_write () in
  let* () = Lwt_io.close output in
  let* () = Lwt_io.close input in
  Lwt.return_unit

let main () =
  let socket_path = ref "" in
  let socket_port = ref 0 in
  let options =
    [
      ( "--pipe",
        Arg.Set_string socket_path,
        "a socket/pipe file name given from vscode" );
      ( "--socket",
        Arg.Set_int socket_port,
        "a socket port number given from vscode" );
    ]
  in
  let usage_msg =
    "Usage: ./main.exe --pipe=[a socket/pipe file name given from vscode] \
     --socket=[a socket port number given from vscode]"
  in
  Arg.parse options (fun _ -> ()) usage_msg;
  (*print_endline (string_of_int !socket_port);*)
  print_endline !socket_path;
  let* () = Lwt_unix.sleep 3. in
  let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
  let sock_addr = Lwt_unix.ADDR_UNIX !socket_path in
  (*let sock_addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, !socket_port) in*)
  print_endline "created server socket";
  let* () =
    Lwt.catch
      (fun () -> Lwt_unix.connect sock sock_addr)
      (function exn -> raise exn)
  in
  print_endline "connected socket";
  let input = Lwt_io.of_fd ~mode:Lwt_io.input sock in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output sock in
  handle_client (input, output)

let () = Lwt_main.run (Lwt.join [ client (); main () ])
(*
let () =
  print_endline "start!";
  Lwt_main.run (main ())
  *)
