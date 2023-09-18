open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Initialize.S

module Make (Repo : Input) : Output = struct
  type input = ClientCapabilities.t
  type output = ServerCapabilities.t

  let client_capabilities = ref (ClientCapabilities.create ())

  let exec c =
    client_capabilities := c;
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~change:Incremental ()))
      ~completionProvider:
        (CompletionOptions.create ~triggerCharacters:[ "i" ]
           ~allCommitCharacters:[] ~resolveProvider:true
           ~completionItem:
             (CompletionOptions.create_completionItem ~labelDetailsSupport:true
                ())
           ~workDoneProgress:false ())
      ()
end
