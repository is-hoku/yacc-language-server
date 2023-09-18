open Language_server.Import

module type S = sig
  (* textDocument/completion *)
  type input = { uri : DocumentUri.t; pos : Position.t }
  type error = { message : string; uri : DocumentUri.t }
  type output = (CompletionItem.t list, error) Result.t

  val exec : input -> output
end
