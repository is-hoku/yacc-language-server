open Language_server.Import

module type S = sig
  type input = ClientCapabilities.t
  type output = ServerCapabilities.t

  val client_capabilities : ClientCapabilities.t ref
  val exec : input -> output
end
