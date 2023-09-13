module type S = sig
  type key
  type value
  type get_input = key
  type not_found_error = { uri : key; message : string }
  type get_output = (value, not_found_error) Result.t

  val get_document : get_input -> get_output

  type register_input = key * Model.Document.t
  type register_output = value

  val register_document : register_input -> register_output
end
