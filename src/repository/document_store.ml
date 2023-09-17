open Language_server.Import

module type S = sig
  type value
  type get_input = Uri.t
  type not_found_error = { uri : Uri.t; message : string }
  type get_output = (value, not_found_error) Result.t

  val get_document : get_input -> get_output

  type register_input = Uri.t * Model.Document.t
  type register_output = value

  val register_document : register_input -> register_output
end
