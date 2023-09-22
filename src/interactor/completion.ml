open Language_server.Import
open Lwt.Syntax

module type Input = Repository.Document_store.S
module type Output = Usecase.Completion.S

module Make (Repo : Input) : Output = struct
  (* textDocument/completion *)
  type input = { uri : DocumentUri.t; pos : Position.t }
  type error = { message : string; uri : DocumentUri.t }
  type output = (CompletionItem.t list, error) Result.t Lwt.t

  let exec (item : input) =
    let* result = Repo.get_document item.uri in
    match result with
    | Result.Ok v -> (
        match v.document with
        | Some d ->
            let text = Text_document.text d.tdoc in
            let pos = Text_document.absolute_position d.tdoc item.pos - 1 in
            if pos >= String.length (Text_document.text d.tdoc) then
              Lwt.return_error
                {
                  message =
                    Printf.sprintf "out of bounds: len=%d pos=%d doc=%s"
                      (String.length text) pos text;
                  uri = item.uri;
                }
            else if String.get (Text_document.text d.tdoc) pos = 'i' then
              Lwt.return_ok
                [
                  CompletionItem.create ~label:"inagaki-lab" ();
                  CompletionItem.create ~label:"inagaki-sensei" ();
                  CompletionItem.create ~label:"india" ();
                  CompletionItem.create ~label:"indonesia" ();
                ]
            else
              Lwt.return_error
                {
                  message = "Invalid Completation";
                  uri = Text_document.documentUri d.tdoc;
                }
        | None ->
            Lwt.return_error { message = "Empty Text Document"; uri = item.uri }
        )
    | Result.Error (Not_found err) ->
        Lwt.return_error { message = "Not Found"; uri = err }
end
