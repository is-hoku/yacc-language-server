open Language_server.Import

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
        (* XXX: Semantic tokens cache is required.*)
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
