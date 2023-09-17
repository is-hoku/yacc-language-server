open Language_server.Import

module type Output = Repository.Document_store.S

type doc = {
  document : Model.Document.t option;
  promotions : int;
      (* The number of promotions for the document *)
      (* XXX: Semantic tokens cache is required.*)
}

module Make : Output = struct
  module Tbl = Hashtbl.Make (struct
    type t = Uri.t

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  type value = doc

  let t = Tbl.create 10

  type get_input = Uri.t
  type not_found_error = { uri : Uri.t; message : string }
  type get_output = (value, not_found_error) Result.t

  let get_document uri =
    match Tbl.find_opt t uri with
    | None -> Result.error { uri; message = "not_found" }
    | Some doc -> Result.ok doc

  type register_input = Uri.t * Model.Document.t
  type register_output = value

  let register_document (uri, m) =
    let doc = { document = Some m; promotions = 0 } in
    Tbl.add t uri doc;
    doc
end
