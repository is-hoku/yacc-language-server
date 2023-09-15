open Language_server.Import

module type Input = Repository.Document_store.S
module type Output = Usecase.Initialize.S

module Make (Repo : Input) : Output = struct
  type input = Jsonrpc.Json.t
  type initialize_error = { message : string; json : Json.t }
  type output = (ServerCapabilities.t, initialize_error) Result.t

  let client_capabilities = ref (ClientCapabilities.create ())

  let initialize json =
    let get_capabilities = function
      | `Assoc fields -> Json.field_exn fields "capabilities" (fun y -> y)
      | _ -> failwith "invalid json structure"
    in
    let capabilities =
      try Result.ok (get_capabilities json) with
      | Failure message -> Result.error { message; json }
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (err, json) ->
          Result.error { message = Printexc.to_string_default err; json }
    in
    match capabilities with
    | Result.Ok json ->
        client_capabilities := ClientCapabilities.t_of_yojson json;
        Result.ok
          (ServerCapabilities.create
             ~textDocumentSync:
               (`TextDocumentSyncOptions
                 (TextDocumentSyncOptions.create ~openClose:true
                    ~change:Incremental ()))
             ~codeActionProvider:(`Bool true) ())
    | Result.Error err -> Result.error err
end
