open Language_server.Import
open Language_server.Io

(*open Language_server.Mock*)
open Lwt.Syntax

let handle_client (input, output) =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Logs.info (fun m -> m "start");
  let rec read_and_write () =
    let* request = IO.read input in
    match request with
    | None -> Lwt.return_unit
    | Some packet ->
        let* () =
          match packet with
          | Notification r -> (
              let notification_method = r.method_ in
              match notification_method with
              | "initialized" -> Logs_lwt.info (fun m -> m "initialized")
              | _ -> Lwt.return_unit)
          | Request r -> (
              let id = r.id in
              let req_method = r.method_ in
              match req_method with
              | "initialize" ->
                  Logs.info (fun m -> m "initialize");
                  IO.write output
                    (Jsonrpc.Packet.t_of_yojson
                       (Jsonrpc.Response.yojson_of_t
                          (Jsonrpc.Response.ok id
                             (InitializeResult.yojson_of_t
                                (InitializeResult.create
                                   ~capabilities:
                                     (ServerCapabilities.create
                                        ~textDocumentSync:
                                          (`TextDocumentSyncOptions
                                            (TextDocumentSyncOptions.create
                                               ~openClose:true
                                               ~change:Incremental ()))
                                        ~codeActionProvider:(`Bool true) ())
                                   ~serverInfo:
                                     (InitializeResult.create_serverInfo
                                        ~name:"yacc-lsp" ())
                                   ())))))
              | _ -> Logs_lwt.info (fun m -> m "invalid method"))
          | Response _ ->
              Logs_lwt.info (fun m -> m "responses aren't supported")
          | Batch_call _ ->
              Logs_lwt.info (fun m -> m "batch requests aren't supported")
          | Batch_response _ ->
              Logs_lwt.info (fun m -> m "batch responses aren't supported")
        in
        read_and_write ()
  in
  let* () = read_and_write () in
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
  let sock_addr = Lwt_unix.ADDR_UNIX !socket_path in
  Lwt_io.with_connection sock_addr handle_client
(*
XXX: comment below when using a mock client
let () =
  Lwt_main.run
    (Lwt.join
       [
         client ();
         (let* () = Lwt_unix.sleep 3. in
          main ());
       ])
*)

let () = Lwt_main.run (main ())
