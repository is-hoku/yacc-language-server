open Language_server.Import

module type Output = Repository.Document_store.S

module Make : Output = struct
  module Tbl = Hashtbl.Make (struct
    type t = Uri.t

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

  type get_input = Uri.t
  type error = Not_found of Uri.t
  type get_output = (value, error) Result.t

  let get_document uri =
    match Tbl.find_opt t uri with
    | None -> Result.error (Not_found uri)
    | Some doc -> Result.ok doc

  type register_input = Uri.t * Model.Document.t
  type register_output = value

  let register_document (uri, m) =
    let doc =
      match Tbl.find_opt t uri with
      | None -> { document = Some m; promotions = 0 }
      | Some d -> { document = Some m; promotions = d.promotions }
    in
    Tbl.replace t uri doc;
    doc
end
