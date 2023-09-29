open Language_server.Import

type value = {
  document : Model.Document.t option;
  promotions : int;
      (* The number of promotions for the document *)
      (* TODO: Semantic tokens cache is required.*)
}

type get_input = DocumentUri.t
type error = Not_found of DocumentUri.t
type get_output = (value, error) Result.t Lwt.t

let get_document uri =
  let uri_string = DocumentUri.to_string uri in
  match uri_string with
  | "file:///interactor/mock/example.y" ->
      Lwt.return
        (Result.ok
           {
             document =
               Some
                 (Model.Document.make
                    (TextDocumentItem.create ~languageId:"yacc" ~text:"i" ~uri
                       ~version:1)
                    `UTF16);
             promotions = 0;
           })
  | _ -> Lwt.return (Result.error (Not_found uri))

type register_input = DocumentUri.t * Model.Document.t
type register_output = value Lwt.t

let register_document (uri, m) =
  match DocumentUri.to_string uri with
  | "file:///interactor/mock/not_found.y" ->
      Lwt.return { document = None; promotions = 0 }
  | _ -> Lwt.return { document = Some m; promotions = 0 }
