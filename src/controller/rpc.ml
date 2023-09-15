open Language_server.Import
open Io
open Lwt.Syntax

module type Input = Usecase.Lsp.S
module type Output = Service.Rpc.S

module Make (UC : Input) : Output = struct
  type request_input = Jsonrpc.Request.t
  type request_output = Jsonrpc.Packet.t

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
        | Some p -> (
            Logs.info (fun m -> m "initialize");
            match
              UC.Initialize.initialize (Jsonrpc.Structured.yojson_of_t p)
            with
            | Result.Ok client_capabilities ->
                make_response_packet req.id
                  (InitializeResult.yojson_of_t
                     (InitializeResult.create ~capabilities:client_capabilities
                        ~serverInfo:
                          (InitializeResult.create_serverInfo ~name:"yacc-lsp"
                             ())
                        ()))
            | Result.Error err ->
                show_message_notification MessageType.Error err.message)
        | None ->
            show_message_notification MessageType.Error
              "initialize parameters are not found")
    | _ -> show_message_notification MessageType.Error "unknown request method"

  type notification_input = Jsonrpc.Notification.t
  type notification_output = Jsonrpc.Packet.t option

  let on_notification (notification : notification_input) =
    match notification.method_ with
    | "initialized" ->
        Logs.info (fun m -> m "initialized");
        None
    | _ ->
        Some
          (show_message_notification MessageType.Error
             "unknown notification method")

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
