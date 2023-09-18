open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Didopen.S

module Make (Repo : Input) : Output = struct
  (* textDocument/didOpen *)
  type input = TextDocumentItem.t
  type error = { message : string; uri : DocumentUri.t }
  type output = (Text_document.t, error) Result.t

  let exec (doc : input) =
    let d = Repo.register_document (doc.uri, Model.Document.make doc) in
    match d.document with
    | Some v -> Result.ok v.tdoc
    | None ->
        Result.error { message = "Cannot open the document"; uri = doc.uri }
end
