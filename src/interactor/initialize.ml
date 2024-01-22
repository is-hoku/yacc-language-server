open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Initialize.S

module Make (Repo : Input) : Output = struct
  type input = unit
  type output = ServerCapabilities.t

  let exec () =
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~change:Incremental
             ~save:(`Bool true) ()))
      ~completionProvider:
        (CompletionOptions.create
           ~triggerCharacters:[ "i" ] (* set " " as triggerCharacters *)
           ~allCommitCharacters:[] ~resolveProvider:true
           ~completionItem:
             (CompletionOptions.create_completionItem ~labelDetailsSupport:true
                ())
           ~workDoneProgress:false ())
      ()
end

let%test_unit _ =
  let module UC = Make (Mock) in
  let _ = UC.exec () in
  ()
