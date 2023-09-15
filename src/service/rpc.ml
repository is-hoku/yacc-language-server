module type S = sig
  type server_input = Lwt_io.input_channel * Lwt_io.output_channel
  type server_output = unit Lwt.t

  val start : server_input -> server_output

  type request_input = Jsonrpc.Request.t
  type request_output = Jsonrpc.Packet.t

  val on_request : request_input -> request_output

  type notification_input = Jsonrpc.Notification.t
  type notification_output = Jsonrpc.Packet.t option

  val on_notification : notification_input -> notification_output
end
