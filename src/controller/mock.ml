open Language_server.Import

module Initialize = struct
  type input = unit
  type output = ServerCapabilities.t

  let exec _ = ServerCapabilities.create ()
end

module DidOpen = struct
  type input = TextDocumentItem.t
  type error = { message : string; uri : DocumentUri.t }
  type output = (Text_document.t, error) Result.t Lwt.t

  let exec (doc : input) =
    Lwt.return_ok
      (Text_document.make ~position_encoding:`UTF16
         (DidOpenTextDocumentParams.create ~textDocument:doc))
end

module DidChange = struct
  type input = {
    uri : DocumentUri.t;
    contents : TextDocumentContentChangeEvent.t list;
  }

  type error = { message : string; uri : DocumentUri.t }
  type output = (Text_document.t, error) Result.t Lwt.t

  let exec (doc : input) =
    match DocumentUri.to_string doc.uri with
    | "file:///controller/mock/example.y" ->
        Lwt.return_ok
          (Text_document.make ~position_encoding:`UTF16
             (DidOpenTextDocumentParams.create
                ~textDocument:
                  (TextDocumentItem.create ~languageId:"yacc" ~text:"i"
                     ~uri:(Uri.of_path "controller/mock/example.y")
                     ~version:1)))
    | _ -> Lwt.return_error { message = "not found"; uri = doc.uri }
end

module Completion = struct
  type input = { uri : DocumentUri.t; pos : Position.t }
  type error = { message : string; uri : DocumentUri.t }
  type output = (CompletionItem.t list, error) Result.t Lwt.t

  let exec (item : input) =
    match DocumentUri.to_string item.uri with
    | "file:///controller/mock/example.y" ->
        if item.pos != Position.create ~character:0 ~line:0 then
          Lwt.return_error { message = "out of bounds"; uri = item.uri }
        else
          Lwt.return_ok
            [
              CompletionItem.create ~label:"example" ();
              CompletionItem.create ~label:"sample" ();
            ]
    | _ -> Lwt.return_error { message = "not found"; uri = item.uri }
end
