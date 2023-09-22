open Language_server.Import
open Io
open Lwt.Syntax

module type Input = Usecase.Lsp.S
module type Output = Service.Rpc.S

module Make (UC : Input) : Output = struct
  type request_input = Jsonrpc.Request.t
  type request_output = Jsonrpc.Packet.t Lwt.t
  type json_error = { message : string; json : Json.t }

  let show_message_notification mtype msg =
    Jsonrpc.Packet.t_of_yojson
      (Jsonrpc.Notification.yojson_of_t
         (Jsonrpc.Notification.create
            ~params:
              (Jsonrpc.Structured.t_of_yojson
                 (ShowMessageParams.yojson_of_t
                    (ShowMessageParams.create ~message:msg ~type_:mtype)))
            ~method_:"window/showMessage" ()))

  let make_response_packet id json =
    Jsonrpc.Packet.t_of_yojson
      (Jsonrpc.Response.yojson_of_t (Jsonrpc.Response.ok id json))

  let on_request (req : request_input) =
    match req.method_ with
    | "initialize" -> (
        match req.params with
        | Some p ->
            Logs.info (fun m -> m "initialize");
            let params =
              InitializeParams.t_of_yojson (Jsonrpc.Structured.yojson_of_t p)
            in
            Lwt.return
              (make_response_packet req.id
                 (InitializeResult.yojson_of_t
                    (InitializeResult.create
                       ~capabilities:(UC.Initialize.exec params.capabilities)
                       ~serverInfo:
                         (InitializeResult.create_serverInfo ~name:"yacc-lsp" ())
                       ())))
        | None ->
            Lwt.return
              (show_message_notification MessageType.Error
                 "parameters are not found"))
    | "textDocument/completion" -> (
        match req.params with
        | Some p -> (
            Logs.info (fun m -> m "completion");
            let params =
              CompletionParams.t_of_yojson (Jsonrpc.Structured.yojson_of_t p)
            in
            let* result =
              UC.Completion.exec
                { uri = params.textDocument.uri; pos = params.position }
            in
            match result with
            | Result.Ok comp ->
                Lwt.return
                  (make_response_packet req.id
                     (`List (List.map CompletionItem.yojson_of_t comp)))
            | Result.Error err ->
                Lwt.return
                  (show_message_notification MessageType.Error
                     (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                        err.message)))
        | None ->
            Lwt.return
              (show_message_notification MessageType.Error
                 "parameters are not found"))
    | _method ->
        Logs.info (fun m ->
            m "unknown: %s"
              (Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req)));
        Lwt.return
          (show_message_notification MessageType.Log
             ("unknown request method: " ^ _method
             ^ Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req)))

  type notification_input = Jsonrpc.Notification.t
  type notification_output = Jsonrpc.Packet.t option Lwt.t

  let on_notification (notification : notification_input) =
    match notification.method_ with
    | "initialized" ->
        Logs.info (fun m -> m "initialized");
        Lwt.return_none
    | "textDocument/didOpen" -> (
        match notification.params with
        | Some p -> (
            Logs.info (fun m -> m "textDocument/didOpen");
            let params =
              DidOpenTextDocumentParams.t_of_yojson
                (Jsonrpc.Structured.yojson_of_t p)
            in
            let* result = UC.DidOpen.exec params.textDocument in
            match result with
            | Result.Ok _ -> Lwt.return_none
            | Result.Error err ->
                Lwt.return_some
                  (show_message_notification MessageType.Error
                     (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                        err.message)))
        | None ->
            Lwt.return_some
              (show_message_notification MessageType.Error
                 "parameters are not found"))
    | "textDocument/didChange" -> (
        match notification.params with
        | Some p -> (
            Logs.info (fun m -> m "textDocument/didChange");
            let params =
              DidChangeTextDocumentParams.t_of_yojson
                (Jsonrpc.Structured.yojson_of_t p)
            in
            let* result =
              UC.DidChange.exec
                {
                  uri = params.textDocument.uri;
                  contents = params.contentChanges;
                }
            in
            match result with
            | Result.Ok _ ->
                (*Logs.info (fun m -> m "%s" (Text_document.text d));*)
                Lwt.return_none
            | Result.Error err ->
                Lwt.return_some
                  (show_message_notification MessageType.Error
                     (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                        err.message)))
        | None ->
            Lwt.return_some
              (show_message_notification MessageType.Error
                 "parameters are not found"))
    | _method ->
        Logs.info (fun m ->
            m "unknown: %s"
              (Yojson.Safe.to_string
                 (Jsonrpc.Notification.yojson_of_t notification)));
        Lwt.return_some
          (show_message_notification MessageType.Log
             ("unknown notification method: " ^ _method))

  type server_input = Lwt_io.input_channel * Lwt_io.output_channel
  type server_output = unit Lwt.t

  let start (input, output) =
    let rec read_and_write () =
      let* request = IO.read input in
      match request with
      | None -> Lwt.return_unit
      | Some packet ->
          let* () =
            match packet with
            | Notification r -> (
                let* result = on_notification r in
                match result with
                | Some p -> IO.write output p
                | None -> Lwt.return_unit)
            | Request r ->
                let* result = on_request r in
                IO.write output result
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
end
