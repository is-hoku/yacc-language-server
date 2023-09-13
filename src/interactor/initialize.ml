open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Initialize.S

module Make (Repo : Input) : Output = struct
  type input = Jsonrpc.Json.t
  type output = ServerCapabilities.t

  let client_capabilities = ref (ClientCapabilities.create ())

  let initialize json =
    client_capabilities := ClientCapabilities.t_of_yojson json;

    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~change:Incremental ()))
      ~codeActionProvider:(`Bool true) ()
end
