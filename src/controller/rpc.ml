open Language_server.Import
open Io
open Lwt.Syntax

module type Input = Usecase.Lsp.S
module type Output = Service.Rpc.S

module Make (UC : Input) : Output = struct
  type request_input = Jsonrpc.Request.t
  type request_output = Jsonrpc.Packet.t
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

  let get_client_capabilities json =
    let get_capabilities = function
      | `Assoc fields ->
          ClientCapabilities.t_of_yojson
            (Json.field_exn fields "capabilities" (fun y -> y))
      | _ -> failwith "invalid json structure"
    in
    try Result.ok (get_capabilities json) with
    | Failure message -> Result.error { message; json }
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (err, json) ->
        Result.error { message = Printexc.to_string_default err; json }

  let get_text_document json =
    let get_tdoc = function
      | `Assoc fields ->
          TextDocumentItem.t_of_yojson
            (Json.field_exn fields "textDocument" (fun y -> y))
      | _ -> failwith "invalid json structure"
    in
    try Result.ok (get_tdoc json) with
    | Failure message -> Result.error { message; json }
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (err, json) ->
        Result.error { message = Printexc.to_string_default err; json }

  let get_versioned_text_document json =
    let get_tdoc = function
      | `Assoc fields ->
          VersionedTextDocumentIdentifier.t_of_yojson
            (Json.field_exn fields "textDocument" (fun y -> y))
      | _ -> failwith "invalid json structure"
    in
    try Result.ok (get_tdoc json) with
    | Failure message -> Result.error { message; json }
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (err, json) ->
        Result.error { message = Printexc.to_string_default err; json }

  let get_change_event json =
    let get_events = function
      | `Assoc fields -> (
          match Json.field_exn fields "contentChanges" (fun y -> y) with
          | `List changes ->
              List.map
                (fun y -> TextDocumentContentChangeEvent.t_of_yojson y)
                changes
          | _ -> failwith "not found contentChanges field")
      | _ -> failwith "invalid json structure"
    in
    try Result.ok (get_events json) with
    | Failure message -> Result.error { message; json }
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (err, json) ->
        Result.error { message = Printexc.to_string_default err; json }

  let on_request (req : request_input) =
    match req.method_ with
    | "initialize" -> (
        match req.params with
        | Some p -> (
            Logs.info (fun m -> m "initialize");
            match
              get_client_capabilities (Jsonrpc.Structured.yojson_of_t p)
            with
            | Result.Ok client_capabilities ->
                make_response_packet req.id
                  (InitializeResult.yojson_of_t
                     (InitializeResult.create
                        ~capabilities:
                          (UC.Initialize.initialize client_capabilities)
                        ~serverInfo:
                          (InitializeResult.create_serverInfo ~name:"yacc-lsp"
                             ())
                        ()))
            | Result.Error err ->
                show_message_notification MessageType.Error err.message)
        | None ->
            show_message_notification MessageType.Error
              "parameters are not found")
    | _method ->
        show_message_notification MessageType.Error
          ("unknown request method: " ^ _method
          ^ Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req))

  type notification_input = Jsonrpc.Notification.t
  type notification_output = Jsonrpc.Packet.t option

  let on_notification (notification : notification_input) =
    match notification.method_ with
    | "initialized" ->
        Logs.info (fun m -> m "initialized");
        None
    | "textDocument/didOpen" -> (
        match notification.params with
        | Some p -> (
            Logs.info (fun m -> m "textDocument/didOpen");
            match get_text_document (Jsonrpc.Structured.yojson_of_t p) with
            | Result.Ok item -> (
                match UC.Textdocument.didopen item with
                | Result.Ok _ -> None
                | Result.Error err ->
                    Some
                      (show_message_notification MessageType.Error
                         (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                            err.message)))
            | Result.Error err ->
                Some (show_message_notification MessageType.Error err.message))
        | None ->
            Some
              (show_message_notification MessageType.Error
                 "parameters are not found"))
    | "textDocument/didChange" -> (
        match notification.params with
        | Some p -> (
            Logs.info (fun m -> m "textDocument/didChange");
            match
              ( get_versioned_text_document (Jsonrpc.Structured.yojson_of_t p),
                get_change_event (Jsonrpc.Structured.yojson_of_t p) )
            with
            | Result.Ok tdoc_id, Result.Ok item -> (
                match
                  UC.Textdocument.didchange
                    { uri = tdoc_id.uri; contents = item }
                with
                | Result.Ok _ ->
                    (*Logs.info (fun m -> m "%s" (Text_document.text d));*)
                    None
                | Result.Error err ->
                    Some
                      (show_message_notification MessageType.Error
                         (Printf.sprintf "%s: %s" (Uri.to_string err.uri)
                            err.message)))
            | Result.Error err1, Result.Error err2 ->
                (* XXX: fix this error handling *)
                Some
                  (show_message_notification MessageType.Error
                     (err1.message ^ "\n" ^ err2.message))
            | Result.Error err, _ ->
                Some (show_message_notification MessageType.Error err.message)
            | _, Result.Error err ->
                Some (show_message_notification MessageType.Error err.message))
        | None ->
            Some
              (show_message_notification MessageType.Error
                 "parameters are not found"))
    | _method ->
        Logs.info (fun m ->
            m "unknown: %s"
              (Yojson.Safe.to_string
                 (Jsonrpc.Notification.yojson_of_t notification)));
        Some
          (show_message_notification MessageType.Error
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
                match on_notification r with
                | Some p -> IO.write output p
                | None -> Lwt.return_unit)
            | Request r -> IO.write output (on_request r)
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
