open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Completion.S

module Make (Repo : Input) : Output = struct
  (* textDocument/completion *)
  type input = { uri : DocumentUri.t; pos : Position.t }
  type error = { message : string; uri : DocumentUri.t }
  type output = (CompletionItem.t list, error) Result.t

  let exec (item : input) =
    match Repo.get_document item.uri with
    | Result.Ok v -> (
        match v.document with
        | Some d ->
            let pos = Text_document.absolute_position d.tdoc item.pos in
            Logs.info (fun m ->
                m "%s: %c"
                  (string_of_int (pos - 1))
                  (String.get (Text_document.text d.tdoc) (pos - 1)));
            if String.get (Text_document.text d.tdoc) (pos - 1) = 'i' then
              Result.ok
                [
                  CompletionItem.create ~label:"inagaki-lab" ();
                  CompletionItem.create ~label:"inagaki-sensei" ();
                  CompletionItem.create ~label:"india" ();
                  CompletionItem.create ~label:"indonesia" ();
                ]
            else
              Result.error
                {
                  message = "Invalid Completation";
                  uri = Text_document.documentUri d.tdoc;
                }
        | None ->
            Result.error { message = "Empty Text Document"; uri = item.uri })
    | Result.Error (Not_found err) ->
        Result.error { message = "Not Found"; uri = err }
end
