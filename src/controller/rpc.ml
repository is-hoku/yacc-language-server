open Language_server.Import
open Io
open Lwt.Syntax

module type Input = Usecase.Lsp.S
module type Output = Service.Rpc.S

module Make (UC : Input) : Output = struct
  type init = Uninitialized | Initialized of { params : InitializeParams.t }
  type state = { init : init }

  let create_state () = { init = Uninitialized }

  let initialize_params state =
    match state.init with
    | Uninitialized -> None
    | Initialized init -> Some init.params

  let initialize t params =
    match t.init with
    | Initialized _ -> None
    | Uninitialized -> Some { init = Initialized { params } }

  type request_input = Jsonrpc.Request.t * state
  type request_output = (Jsonrpc.Packet.t * state) Lwt.t
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

  let on_request ((req, s) : request_input) =
    let params = req.params in
    let state = initialize_params s in
    match req.method_ with
    | "initialize" -> (
        Logs.info (fun m -> m "initialize");
        match (params, state) with
        (* Since initialize requests are sent only once, initialize requests are allowed only when uninitialized *)
        | Some p, None -> (
            let params =
              InitializeParams.t_of_yojson (Jsonrpc.Structured.yojson_of_t p)
            in
            match initialize s params with
            | Some new_state ->
                Lwt.return
                  ( make_response_packet req.id
                      (InitializeResult.yojson_of_t
                         (InitializeResult.create
                            ~capabilities:
                              (UC.Initialize.exec params.capabilities)
                            ~serverInfo:
                              (InitializeResult.create_serverInfo
                                 ~name:"yacc-lsp" ())
                            ())),
                    new_state )
            | None ->
                Lwt.return
                  ( show_message_notification MessageType.Error
                      "failed initialize",
                    s ))
        | _, Some _ ->
            Lwt.return
              ( show_message_notification MessageType.Error "already initialized",
                s )
        | None, _ ->
            Lwt.return
              ( show_message_notification MessageType.Error
                  "parameters are not found",
                s ))
    | "textDocument/completion" -> (
        Logs.info (fun m -> m "completion");
        match (params, state) with
        | Some p, Some _ -> (
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
                  ( make_response_packet req.id
                      (`List (List.map CompletionItem.yojson_of_t comp)),
                    s )
            | Result.Error err ->
                Lwt.return
                  ( show_message_notification MessageType.Error
                      (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                         err.message),
                    s ))
        | _, None ->
            Lwt.return
              (show_message_notification MessageType.Error "uninitialized", s)
        | None, _ ->
            Lwt.return
              ( show_message_notification MessageType.Error
                  "parameters are not found",
                s ))
    | _method ->
        Logs.info (fun m ->
            m "unknown: %s"
              (Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req)));
        Lwt.return
          ( show_message_notification MessageType.Log
              ("unknown request method: " ^ _method
              ^ Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req)),
            s )

  type notification_input = Jsonrpc.Notification.t * state
  type notification_output = (Jsonrpc.Packet.t option * state) Lwt.t

  let on_notification ((notification, s) : notification_input) =
    let params = notification.params in
    let state = initialize_params s in
    match notification.method_ with
    | "initialized" -> (
        Logs.info (fun m -> m "initialized");
        match state with
        | Some _ -> Lwt.return (None, s)
        | None ->
            Lwt.return
              ( Some
                  (show_message_notification MessageType.Error "uninitialized"),
                s ))
    | "textDocument/didOpen" -> (
        Logs.info (fun m -> m "textDocument/didOpen");
        match (params, state) with
        | Some p, Some _ -> (
            Logs.info (fun m -> m "textDocument/didOpen");
            let params =
              DidOpenTextDocumentParams.t_of_yojson
                (Jsonrpc.Structured.yojson_of_t p)
            in
            let* result = UC.DidOpen.exec params.textDocument in
            match result with
            | Result.Ok _ -> Lwt.return (None, s)
            | Result.Error err ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Error
                         (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                            err.message)),
                    s ))
        | _, None ->
            Lwt.return
              ( Some
                  (show_message_notification MessageType.Error "uninitialized"),
                s )
        | None, _ ->
            Lwt.return
              ( Some
                  (show_message_notification MessageType.Error
                     "parameters are not found"),
                s ))
    | "textDocument/didChange" -> (
        Logs.info (fun m -> m "textDocument/didChange");
        match (params, state) with
        | Some p, Some _ -> (
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
                Lwt.return (None, s)
            | Result.Error err ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Error
                         (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                            err.message)),
                    s ))
        | _, None ->
            Lwt.return
              ( Some
                  (show_message_notification MessageType.Error "uninitialized"),
                s )
        | None, _ ->
            Lwt.return
              ( Some
                  (show_message_notification MessageType.Error
                     "parameters are not found"),
                s ))
    | _method ->
        Logs.info (fun m ->
            m "unknown: %s"
              (Yojson.Safe.to_string
                 (Jsonrpc.Notification.yojson_of_t notification)));
        Lwt.return
          ( Some
              (show_message_notification MessageType.Log
                 ("unknown notification method: " ^ _method)),
            s )

  type server_input = Lwt_io.input_channel * Lwt_io.output_channel
  type server_output = unit Lwt.t

  let start (input, output) =
    let s = create_state () in
    let rec read_and_write state =
      let* request = IO.read input in
      match request with
      | None -> Lwt.return_unit
      | Some packet ->
          let* new_state =
            match packet with
            | Notification r -> (
                let* result = on_notification (r, state) in
                match result with
                | Some p, new_state ->
                    let* () = IO.write output p in
                    Lwt.return new_state
                | None, new_state -> Lwt.return new_state)
            | Request r ->
                let* result = on_request (r, state) in
                let* () = IO.write output (fst result) in
                Lwt.return (snd result)
            | Response _ ->
                let* () =
                  Logs_lwt.info (fun m -> m "responses aren't supported")
                in
                Lwt.return state
            | Batch_call _ ->
                let* () =
                  Logs_lwt.info (fun m -> m "batch requests aren't supported")
                in
                Lwt.return state
            | Batch_response _ ->
                let* () =
                  Logs_lwt.info (fun m -> m "batch responses aren't supported")
                in
                Lwt.return state
          in
          read_and_write new_state
    in
    let* () = read_and_write s in
    Lwt.return_unit
end
