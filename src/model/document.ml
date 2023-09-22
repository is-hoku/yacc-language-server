open Language_server.Import

module Syntax = struct
  type t = Yacc

  let human_name = function Yacc -> "Yacc"
  let all = [ ("yacc", Yacc) ]

  let of_fname p =
    match Filename.extension p with
    | ".y" -> Yacc
    | ext ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make ~code:InvalidRequest
             ~message:"unsupported file extension"
             ~data:(`Assoc [ ("extension", `String ext) ])
             ())

  let to_language_id x =
    List.find_map (fun (k, v) -> if v = x then Option.some k else None) all

  let of_text_document (td : Text_document.t) =
    try List.assoc (Text_document.languageId td) all
    with Not_found -> Text_document.documentUri td |> Uri.to_path |> of_fname
end

type t = { tdoc : Text_document.t; syntax : Syntax.t }

let syntax m = m.syntax

let make doc =
  let tdoc =
    Text_document.make ~position_encoding:`UTF8
      (DidOpenTextDocumentParams.create ~textDocument:doc)
  in
  { tdoc; syntax = Syntax.of_text_document tdoc }
