open Language_server.Import

module type S = sig
  type input = Jsonrpc.Json.t
  type output = ServerCapabilities.t

  val client_capabilities : ClientCapabilities.t ref
  val initialize : input -> output
end
