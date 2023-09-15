open Language_server.Import

module type S = sig
  type input = Jsonrpc.Json.t
  type initialize_error = { message : string; json : Json.t }
  type output = (ServerCapabilities.t, initialize_error) Result.t

  val client_capabilities : ClientCapabilities.t ref
  val initialize : input -> output
end
