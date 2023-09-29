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
    match result with
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
            Lwt.return_ok tdoc
        | None ->
            Lwt.return_error { message = "Empty Text Document"; uri = doc.uri })
    | Result.Error (Not_found err) ->
        Lwt.return_error { message = "Not Found"; uri = err }
end

let%test _ =
  let module UC = Make (Mock) in
  let result =
    Lwt_main.run
      (UC.exec
         {
           uri = DocumentUri.of_path "/interactor/mock/example.y";
           contents =
             [
               TextDocumentContentChangeEvent.create
                 ~range:
                   (Range.create
                      ~end_:(Position.create ~character:2 ~line:0)
                      ~start:(Position.create ~character:1 ~line:0))
                 ~rangeLength:1 ~text:"in" ();
             ];
         })
  in
  match result with
  | Result.Ok _ -> true
  | Result.Error err ->
      print_endline
        (Printf.sprintf "ERROR: %s in %s" err.message (Uri.to_string err.uri));
      false
