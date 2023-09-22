open Language_server.Import

module type S = sig
  (* textDocument/didOpen *)
  type input = TextDocumentItem.t
  type error = { message : string; uri : DocumentUri.t }
  type output = (Text_document.t, error) Result.t Lwt.t

  val exec : input -> output
end
