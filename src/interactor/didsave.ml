open Language_server.Import
open Lwt.Syntax
open Static_analyzer

module type Input = Repository.Document_store.S
module type Output = Usecase.Didsave.S

module Make (Repo : Input) : Output = struct
  (* textDocument/didSave *)
  type input = { uri : DocumentUri.t }
  type error = { message : string; uri : DocumentUri.t }
  type output = (PublishDiagnosticsParams.t, error) Result.t Lwt.t

  let exec (doc : input) =
    let* result = Repo.get_document doc.uri in
    match result with
    | Result.Ok v -> (
        match v.document with
        | Some d ->
            let lexbuf = Lexing.from_string (Text_document.text d.tdoc) in
            let () =
              lexbuf.lex_curr_p <-
                {
                  lexbuf.lex_curr_p with
                  pos_fname = Uri.to_string (Text_document.documentUri d.tdoc);
                }
            in
            let _ =
              Parser.entry
                (Syntax.Incremental.input lexbuf.Lexing.lex_curr_p)
                Lexer.read_token lexbuf
            in
            let uri = Text_document.documentUri d.tdoc in
            let version = Text_document.version d.tdoc in
            let diagnostics = Types.get_diagnostics () in
            let output =
              PublishDiagnosticsParams.create ~diagnostics ~uri ~version ()
            in
            Lwt.return_ok output
        | None ->
            Lwt.return_error { message = "Empty Text Document"; uri = doc.uri })
    | Result.Error (Not_found err) ->
        Lwt.return_error { message = "Not Found"; uri = err }
end
