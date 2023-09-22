open Language_server.Import
open Lwt.Syntax

module type Input = Repository.Document_store.S
module type Output = Usecase.Didchange.S

module Make (Repo : Input) : Output = struct
  (* textDocument/didChange *)
  type input = {
    uri : DocumentUri.t;
    contents : TextDocumentContentChangeEvent.t list;
  }

  type error = { message : string; uri : DocumentUri.t }
  type output = (Text_document.t, error) Result.t Lwt.t

  let exec (doc : input) =
    let* result = Repo.get_document doc.uri in
    Lwt.return
      (match result with
      | Result.Ok v -> (
          match v.document with
          | Some d ->
              let tdoc =
                Text_document.apply_content_changes d.tdoc doc.contents
              in
              let _ =
                Repo.register_document
                  ( Text_document.documentUri tdoc,
                    { tdoc; syntax = Model.Document.syntax d } )
              in
              Result.ok tdoc
          | None ->
              Result.error { message = "Empty Text Document"; uri = doc.uri })
      | Result.Error (Not_found err) ->
          Result.error { message = "Not Found"; uri = err })
end
