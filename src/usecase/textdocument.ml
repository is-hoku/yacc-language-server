open Language_server.Import

module type S = sig
  type textdocument_error = { message : string; uri : Uri.t }
  type didopen_input = TextDocumentItem.t
  type didopen_output = (Text_document.t, textdocument_error) Result.t

  val didopen : didopen_input -> didopen_output

  type didchange_input = {
    uri : Uri.t;
    contents : TextDocumentContentChangeEvent.t list;
  }

  type didchange_output = (Text_document.t, textdocument_error) Result.t

  val didchange : didchange_input -> didchange_output
end
