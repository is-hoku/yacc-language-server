open Language_server.Import
open Io
open Lwt.Syntax

module type Input = Usecase.Lsp.S
module type Output = Service.Rpc.S

module Make (UC : Input) : Output = struct
  type request_input = Jsonrpc.Request.t
  type request_output = Jsonrpc.Packet.t

  let on_request (req : request_input) =
    match req.method_ with
    | "initialize" -> (
        match req.params with
        | Some p ->
            Logs.info (fun m -> m "initialize");
            Jsonrpc.Packet.t_of_yojson
              (Jsonrpc.Response.yojson_of_t
                 (Jsonrpc.Response.ok req.id
                    (InitializeResult.yojson_of_t
                       (InitializeResult.create
                          ~capabilities:
                            (UC.Initialize.initialize
                               (Jsonrpc.Structured.yojson_of_t p))
                          ~serverInfo:
                            (InitializeResult.create_serverInfo ~name:"yacc-lsp"
                               ())
                          ()))))
        | None -> Jsonrpc.Packet.t_of_yojson (`Assoc []))
    | _ -> Jsonrpc.Packet.t_of_yojson (`Assoc [])

  type notification_input = Jsonrpc.Notification.t
  type notification_output = Jsonrpc.Packet.t

  let on_notification (notification : notification_input) =
    match notification.method_ with
    | "initialized" ->
        Logs.info (fun m -> m "initialized");
        Jsonrpc.Packet.t_of_yojson (`Assoc [])
    | _ -> Jsonrpc.Packet.t_of_yojson (`Assoc [])

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
            | Notification r -> IO.write output (on_notification r)
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
