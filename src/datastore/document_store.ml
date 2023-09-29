open Language_server.Import
open Lwt.Syntax

module type Output = Repository.Document_store.S

module Make : Output = struct
  module Tbl = Hashtbl.Make (struct
    type t = DocumentUri.t

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  type value = {
    document : Model.Document.t option;
    promotions : int;
        (* The number of promotions for the document *)
        (* TODO: Semantic tokens cache is required.*)
  }

  let t = Tbl.create 10
  let mutex = Lwt_mutex.create ()

  type get_input = DocumentUri.t
  type error = Not_found of DocumentUri.t
  type get_output = (value, error) Result.t Lwt.t

  let get_document uri =
    Lwt_mutex.with_lock mutex (fun () ->
        match Tbl.find_opt t uri with
        | None -> Lwt.return (Result.error (Not_found uri))
        | Some doc -> Lwt.return (Result.ok doc))

  type register_input = DocumentUri.t * Model.Document.t
  type register_output = value Lwt.t

  let register_document (uri, m) =
    Lwt_mutex.with_lock mutex (fun () ->
        let doc =
          match Tbl.find_opt t uri with
          | None -> { document = Some m; promotions = 0 }
          | Some d -> { document = Some m; promotions = d.promotions }
        in
        Tbl.replace t uri doc;
        Lwt.return doc)
end

let%test "OK: valid input" =
  let module Repo = Make in
  let result =
    Lwt_main.run
      (let* _ =
         Repo.register_document
           ( DocumentUri.of_path "/document_store/test.y",
             Model.Document.make
               (TextDocumentItem.create ~languageId:"yacc" ~text:"i"
                  ~uri:(Uri.of_path "/document_store/test.y")
                  ~version:1)
               `UTF16 )
       in
       Repo.get_document (DocumentUri.of_path "/document_store/test.y"))
  in
  match result with Result.Ok _ -> true | Result.Error _ -> false

let%test "ERROR: not found" =
  let module Repo = Make in
  let result =
    Lwt_main.run
      (Repo.get_document (DocumentUri.of_path "/document_store/not_found.y"))
  in
  match result with Result.Ok _ -> false | Result.Error _ -> true
