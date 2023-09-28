open Language_server.Import

module type S = sig
  type input = unit
  type output = ServerCapabilities.t

  val exec : input -> output
end
