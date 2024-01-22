open Language_server.Import

module type S = sig
  (* textDocument/didSave *)
  type input = { uri : DocumentUri.t }
  type error = { message : string; uri : DocumentUri.t }
  type output = (PublishDiagnosticsParams.t, error) Result.t Lwt.t

  val exec : input -> output
end
