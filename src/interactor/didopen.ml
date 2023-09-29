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

let%test _ =
  let module UC = Make (Mock) in
  let result =
    Lwt_main.run
      (UC.exec
         (TextDocumentItem.create ~languageId:"yacc" ~text:"hogehoge"
            ~uri:(DocumentUri.of_path "/interactor/mock/example.y")
            ~version:1))
  in
  match result with
  | Result.Ok _ -> true
  | Result.Error err ->
      print_endline
        (Printf.sprintf "ERROR: %s in %s" err.message (Uri.to_string err.uri));
      false
