open Language_server.Import
open Io
open Lwt.Syntax

module type Input = Usecase.Lsp.S
module type Output = Service.Rpc.S

module Make (UC : Input) : Output = struct
  type init = Uninitialized | Initialized of { params : InitializeParams.t }
  type client_capabilities = ClientCapabilities.t
  type running_status = Running | Shutdown | Exit of int

  type state = {
    init : init;
    client_capabilities : client_capabilities;
    running_status : running_status;
  }

  let create_state () =
    {
      init = Uninitialized;
      client_capabilities = ClientCapabilities.create ();
      running_status = Running;
    }

  let get_initialize_params state =
    match state.init with
    | Uninitialized -> None
    | Initialized init -> Some init.params

  let initialize s params =
    match s.init with
    | Initialized _ -> None
    | Uninitialized -> Some { s with init = Initialized { params } }

  let get_client_capabilities s = s.client_capabilities
  let update_client_capabilities s c = { s with client_capabilities = c }
  let get_running_status s = s.running_status
  let shutdown s = { s with running_status = Shutdown }
  let exit_status s code = { s with running_status = Exit code }

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

  let response_ok id json =
    Jsonrpc.Packet.t_of_yojson
      (Jsonrpc.Response.yojson_of_t (Jsonrpc.Response.ok id json))

  let response_error id err =
    Jsonrpc.Packet.t_of_yojson
      (Jsonrpc.Response.yojson_of_t (Jsonrpc.Response.error id err))

  let on_request ((req, s) : request_input) =
    let id = req.id in
    let params = req.params in
    match get_running_status s with
    | Shutdown | Exit _ ->
        Lwt.return
          ( response_error id
              (Jsonrpc.Response.Error.make ~code:InvalidRequest
                 ~message:"server is shut down" ()),
            s )
    | Running -> (
        let is_initialized = get_initialize_params s in
        (* TODO: Check client capabilities when adding methods *)
        match req.method_ with
        | "initialize" -> (
            Logs.info (fun m -> m "initialize");
            match params with
            (* Since initialize requests are sent only once, initialize requests are allowed only when uninitialized *)
            | Some p -> (
                let params =
                  (* TODO: catch the invalid structured value error *)
                  InitializeParams.t_of_yojson
                    (Jsonrpc.Structured.yojson_of_t p)
                in
                match initialize s params with
                | Some new_state ->
                    let new_state =
                      update_client_capabilities new_state params.capabilities
                    in
                    Lwt.return
                      ( response_ok id
                          (InitializeResult.yojson_of_t
                             (InitializeResult.create
                                ~capabilities:(UC.Initialize.exec ())
                                ~serverInfo:
                                  (InitializeResult.create_serverInfo
                                     ~name:"yacc-lsp" ())
                                ())),
                        new_state )
                | None ->
                    Lwt.return
                      ( response_error id
                          (Jsonrpc.Response.Error.make ~code:InvalidRequest
                             ~message:"already initialized" ()),
                        s ))
            | None ->
                Lwt.return
                  ( response_error id
                      (Jsonrpc.Response.Error.make ~code:InvalidParams
                         ~message:"parameters are not found" ()),
                    s ))
        | "textDocument/completion" -> (
            Logs.info (fun m -> m "completion");
            match (params, is_initialized) with
            | Some p, Some _ -> (
                let params =
                  CompletionParams.t_of_yojson
                    (Jsonrpc.Structured.yojson_of_t p)
                in
                let* result =
                  UC.Completion.exec
                    { uri = params.textDocument.uri; pos = params.position }
                in
                match result with
                | Result.Ok comp ->
                    Lwt.return
                      ( response_ok id
                          (`List (List.map CompletionItem.yojson_of_t comp)),
                        s )
                | Result.Error err ->
                    Lwt.return
                      ( response_error id
                          (Jsonrpc.Response.Error.make ~code:InternalError
                             ~message:
                               (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                                  err.message)
                             ()),
                        s ))
            | _, None ->
                Lwt.return
                  ( response_error id
                      (Jsonrpc.Response.Error.make ~code:ServerNotInitialized
                         ~message:"uninitialized" ()),
                    s )
            | None, _ ->
                Lwt.return
                  ( response_error id
                      (Jsonrpc.Response.Error.make ~code:InvalidParams
                         ~message:"parameters are not found" ()),
                    s ))
        | "shutdown" -> (
            Logs.info (fun m -> m "shutdown");
            match is_initialized with
            | None ->
                Lwt.return
                  ( response_error id
                      (Jsonrpc.Response.Error.make ~code:ServerNotInitialized
                         ~message:"uninitialized" ()),
                    s )
            | Some _ -> Lwt.return (response_ok id `Null, shutdown s))
        | _method -> (
            Logs.info (fun m ->
                m "unknown: %s"
                  (Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req)));
            match is_initialized with
            | None ->
                Lwt.return
                  ( response_error id
                      (Jsonrpc.Response.Error.make ~code:ServerNotInitialized
                         ~message:"uninitialized" ()),
                    s )
            | Some _ ->
                Lwt.return
                  ( response_error id
                      (Jsonrpc.Response.Error.make ~code:MethodNotFound
                         ~message:"unknown request method" ()),
                    s )))

  type notification_input = Jsonrpc.Notification.t * state
  type notification_output = (Jsonrpc.Packet.t option * state) Lwt.t

  let on_notification ((notification, s) : notification_input) =
    (* TODO: add supports for willSave, willSaveWaitUntil, and DidSave notifications *)
    let params = notification.params in
    let is_initialized = get_initialize_params s in
    (* TODO: Check client capabilities when adding methods *)
    match get_running_status s with
    | Exit _ -> (* ignore notifications *) Lwt.return (None, s)
    | Shutdown -> (
        match notification.method_ with
        | "exit" ->
            Logs.info (fun m -> m "exit");
            (* exit notification success with code 0 *)
            Lwt.return (None, exit_status s 0)
        | _ ->
            (* ignore notifications other than the exit notification after receiving the shutdown request *)
            Lwt.return (None, s))
    | Running -> (
        match notification.method_ with
        | "initialized" ->
            Logs.info (fun m -> m "initialized");
            (* The server can use the initialized notification, for example, to dynamically register capabilities. *)
            Lwt.return (None, s)
        | "textDocument/didOpen" -> (
            Logs.info (fun m -> m "textDocument/didOpen");
            match (params, is_initialized) with
            | Some p, Some _ -> (
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
                          (show_message_notification MessageType.Log
                             (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                                err.message)),
                        s ))
            | _, None ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Log "uninitialized"),
                    s )
            | None, _ ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Log
                         "parameters are not found"),
                    s ))
        | "textDocument/didChange" -> (
            Logs.info (fun m -> m "textDocument/didChange");
            match (params, is_initialized) with
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
                | Result.Ok _ -> Lwt.return (None, s)
                | Result.Error err ->
                    Lwt.return
                      ( Some
                          (show_message_notification MessageType.Log
                             (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                                err.message)),
                        s ))
            | _, None ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Log "uninitialized"),
                    s )
            | None, _ ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Log
                         "parameters are not found"),
                    s ))
        | "textDocument/didSave" -> (
            Logs.info (fun m -> m "textDocument/didSave");
            match (params, is_initialized) with
            | Some p, Some _ -> (
                let params =
                  DidSaveTextDocumentParams.t_of_yojson
                    (Jsonrpc.Structured.yojson_of_t p)
                in
                let* result =
                  UC.DidSave.exec { uri = params.textDocument.uri }
                in
                match result with
                | Result.Ok diagnostics ->
                    Lwt.return
                      ( Some
                          (Jsonrpc.Packet.t_of_yojson
                             (Jsonrpc.Notification.yojson_of_t
                                (Jsonrpc.Notification.create
                                   ~params:
                                     (Jsonrpc.Structured.t_of_yojson
                                        (PublishDiagnosticsParams.yojson_of_t
                                           diagnostics))
                                   ~method_:"textDocument/publishDiagnostics" ()))),
                        s )
                | Result.Error err ->
                    Lwt.return
                      ( Some
                          (show_message_notification MessageType.Log
                             (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                                err.message)),
                        s ))
            | _, None ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Log "uninitialized"),
                    s )
            | None, _ ->
                Lwt.return
                  ( Some
                      (show_message_notification MessageType.Log
                         "parameters are not found"),
                    s ))
        | "exit" ->
            Logs.info (fun m -> m "exit");
            (* exit notification fail with code 1 *)
            Lwt.return (None, exit_status s 1)
        | _method ->
            Logs.info (fun m ->
                m "unknown: %s"
                  (Yojson.Safe.to_string
                     (Jsonrpc.Notification.yojson_of_t notification)));
            Lwt.return
              ( Some
                  (show_message_notification MessageType.Log
                     ("unknown notification method: " ^ _method)),
                s ))

  type server_input = Lwt_io.input_channel * Lwt_io.output_channel
  type server_output = unit Lwt.t

  let start (input, output) =
    let s = create_state () in
    let rec read_and_write state =
      (* TODO: process requests in parallel using Lwt.async etc. *)
      let* request = IO.read input in
      match request with
      | None -> Lwt.return_unit
      | Some packet -> (
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
          match get_running_status new_state with
          | Exit code -> exit code
          | _ -> read_and_write new_state)
    in
    let* () = read_and_write s in
    Lwt.return_unit
end

let%test_module "tests for the initialize request" =
  (module struct
    module SV = Make (Mock)

    let%test "OK: valid initialize request" =
      let params =
        `Assoc
          [
            ("processId", `Null); ("rootUri", `Null); ("capabilities", `Assoc []);
          ]
      in
      let _packet, state =
        Lwt_main.run
          (SV.on_request
             ( Jsonrpc.Request.create ~id:(`Int 0) ~method_:"initialize" ~params
                 (),
               SV.create_state () ))
      in
      match SV.get_initialize_params state with None -> false | Some _ -> true

    let%test "OK: update client capabilities" =
      let params =
        `Assoc
          [
            ("processId", `Null);
            ("rootUri", `Null);
            ( "capabilities",
              `Assoc
                [
                  ( "testDocument",
                    `Assoc
                      [
                        ("synchronization", `Assoc [ ("willSave", `Bool true) ]);
                      ] );
                ] );
          ]
      in
      let _packet, state =
        Lwt_main.run
          (SV.on_request
             ( Jsonrpc.Request.create ~id:(`Int 0) ~method_:"initialize" ~params
                 (),
               SV.create_state () ))
      in
      let capabilities = SV.get_client_capabilities state in
      if
        capabilities
        = ClientCapabilities.t_of_yojson (Jsonrpc.Structured.yojson_of_t params)
      then true
      else false

    let%test "ERROR: already initialized" =
      let state = SV.create_state () in
      let params =
        `Assoc
          [
            ("processId", `Null); ("rootUri", `Null); ("capabilities", `Assoc []);
          ]
      in
      let packet, _state =
        Lwt_main.run
          (SV.on_request
             ( Jsonrpc.Request.create ~id:(`Int 0) ~method_:"initialize" ~params
                 (),
               {
                 state with
                 init =
                   SV.Initialized
                     {
                       params =
                         InitializeParams.create
                           ~capabilities:(ClientCapabilities.create ())
                           ();
                     };
               } ))
      in
      match packet with
      | Response resp -> (
          match resp.result with
          | Result.Error { code = InvalidRequest; message = _; data = _ } ->
              true
          | _ -> false)
      | _ -> false

    let%test "ERROR: server is shut down" =
      let state = SV.create_state () in
      let packet, _state =
        Lwt_main.run
          (SV.on_request
             ( Jsonrpc.Request.create ~id:(`Int 0) ~method_:"initialize" (),
               { state with running_status = SV.Shutdown } ))
      in
      match packet with
      | Response resp -> (
          match resp.result with
          | Result.Error { code = InvalidRequest; message = _; data = _ } ->
              true
          | _ -> false)
      | _ -> false
  end)

let%test_module "tests for the exit notification" =
  (module struct
    module SV = Make (Mock)

    let%test "OK: valid exit notification" =
      let state = SV.create_state () in
      let _packet, state =
        Lwt_main.run
          (SV.on_notification
             ( Jsonrpc.Notification.create ~method_:"exit" (),
               { state with running_status = SV.Shutdown } ))
      in
      match SV.get_running_status state with Exit 0 -> true | _ -> false

    let%test "ERROR: exit notification is received before shut down" =
      let _packet, state =
        Lwt_main.run
          (SV.on_notification
             (Jsonrpc.Notification.create ~method_:"exit" (), SV.create_state ()))
      in
      match SV.get_running_status state with Exit 1 -> true | _ -> false
  end)

let%test_module "tests for the initialized notification" =
  (module struct
    module SV = Make (Mock)

    let%test "OK: valid initialized notification" =
      let state = SV.create_state () in
      let packet, _state =
        Lwt_main.run
          (SV.on_notification
             ( Jsonrpc.Notification.create ~method_:"initialized" (),
               {
                 state with
                 init =
                   SV.Initialized
                     {
                       params =
                         InitializeParams.create
                           ~capabilities:(ClientCapabilities.create ())
                           ();
                     };
               } ))
      in
      match packet with None -> true | Some _ -> false
  end)
