open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Textdocument.S

module Make (Repo : Input) : Output = struct
  type didopen_input = TextDocumentItem.t
  type document_error = { message : string; document : string }
  type didopen_output = unit

  let didopen (doc : TextDocumentItem.t) =
    let _ = Repo.register_document (doc.uri, Model.Document.make doc) in
    ()
end
