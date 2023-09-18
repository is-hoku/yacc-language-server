open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Textdocument.S

module Make (Repo : Input) : Output = struct
  type textdocument_error = { message : string; uri : Uri.t }
  type didopen_input = TextDocumentItem.t
  type didopen_output = (Text_document.t, textdocument_error) Result.t

  let didopen (doc : TextDocumentItem.t) =
    let d = Repo.register_document (doc.uri, Model.Document.make doc) in
    match d.document with
    | Some v -> Result.ok v.tdoc
    | None ->
        Result.error { message = "Cannot open the document"; uri = doc.uri }

  type didchange_input = {
    uri : Uri.t;
    contents : TextDocumentContentChangeEvent.t list;
  }

  type didchange_output = (Text_document.t, textdocument_error) Result.t

  let didchange doc =
    match Repo.get_document doc.uri with
    | Result.Ok v -> (
        match v.document with
        | Some d ->
            let tdoc =
              Text_document.apply_content_changes d.tdoc doc.contents
            in
            Result.ok tdoc
        | None ->
            Result.error { message = "Empty Text Document"; uri = doc.uri })
    | Result.Error (Not_found err) ->
        Result.error { message = "Not Found"; uri = err }
end
