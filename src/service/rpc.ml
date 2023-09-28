open Language_server.Import

module type S = sig
  type init = Uninitialized | Initialized of { params : InitializeParams.t }
  type client_capabilities = ClientCapabilities.t
  type state = { init : init; client_capabilities : client_capabilities }

  val create_state : unit -> state
  val get_initialize_params : state -> InitializeParams.t option
  val initialize : state -> InitializeParams.t -> state option
  val get_client_capabilities : state -> ClientCapabilities.t
  val update_client_capabilities : state -> ClientCapabilities.t -> state

  type server_input = Lwt_io.input_channel * Lwt_io.output_channel
  type server_output = unit Lwt.t

  val start : server_input -> server_output

  type request_input = Jsonrpc.Request.t * state
  type request_output = (Jsonrpc.Packet.t * state) Lwt.t
  type json_error = { message : string; json : Jsonrpc.Json.t }

  val on_request : request_input -> request_output

  type notification_input = Jsonrpc.Notification.t * state
  type notification_output = (Jsonrpc.Packet.t option * state) Lwt.t

  val on_notification : notification_input -> notification_output
end
