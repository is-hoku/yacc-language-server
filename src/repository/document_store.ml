open Language_server.Import

module type S = sig
  type value = {
    document : Model.Document.t option;
    promotions : int;
        (* The number of promotions for the document *)
        (* XXX: Semantic tokens cache is required.*)
  }

  type get_input = DocumentUri.t
  type error = Not_found of DocumentUri.t
  type get_output = (value, error) Result.t Lwt.t

  val get_document : get_input -> get_output

  type register_input = DocumentUri.t * Model.Document.t
  type register_output = value Lwt.t

  val register_document : register_input -> register_output
end
