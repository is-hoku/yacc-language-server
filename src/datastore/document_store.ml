module type Input = Hashtbl.S
module type Output = Repository.Document_store.S

type doc = {
  document : Model.Document.t option;
  promotions : int;
      (* The number of promotions for the document *)
      (* XXX: Semantic tokens cache is required.*)
}

module Make (Tbl : Input) : Output = struct
  type key = Tbl.key
  type value = doc

  let t = Tbl.create 10

  type get_input = key
  type not_found_error = { uri : Tbl.key; message : string }
  type get_output = (value, not_found_error) Result.t

  let get_document uri =
    match Tbl.find_opt t uri with
    | None -> Result.error { uri; message = "not_found" }
    | Some doc -> Result.ok doc

  type register_input = key * Model.Document.t
  type register_output = value

  let register_document (uri, m) =
    let doc = { document = Some m; promotions = 0 } in
    Tbl.add t uri doc;
    doc
end
