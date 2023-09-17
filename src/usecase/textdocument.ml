open Language_server.Import

module type S = sig
  type didopen_input = TextDocumentItem.t
  type document_error = { message : string; document : string }
  type didopen_output = unit

  val didopen : didopen_input -> didopen_output
end
