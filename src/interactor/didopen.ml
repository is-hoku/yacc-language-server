open Language_server.Import
open Lwt.Syntax

module type Input = Repository.Document_store.S
module type Output = Usecase.Didopen.S

module Make (Repo : Input) : Output = struct
  (* textDocument/didOpen *)
  type input = TextDocumentItem.t
  type error = { message : string; uri : DocumentUri.t }
  type output = (Text_document.t, error) Result.t Lwt.t

  let exec (doc : input) =
    (* XXX: change position encoding value based on client capabilities *)
    let* d = Repo.register_document (doc.uri, Model.Document.make doc `UTF16) in
    match d.document with
    | Some v -> Lwt.return_ok v.tdoc
    | None ->
        Lwt.return_error { message = "Cannot open the document"; uri = doc.uri }
end
