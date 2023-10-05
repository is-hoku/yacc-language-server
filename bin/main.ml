let main () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Logs.info (fun m -> m "start");
  let socket_path = ref "" in
  let socket_port = ref 0 in
  let options =
    [
      ( "--pipe",
        Arg.Set_string socket_path,
        "use pipes (Windows) or socket files (Linux, Mac) as the communication \
         channel. The pipe / socket file name is passed as the next arg or \
         with --pipe=." );
      ( "--socket",
        Arg.Set_int socket_port,
        "uses a socket as the communication channel. The port is passed as \
         next arg or with --port=." );
    ]
  in
  let usage_msg =
    "Usage: ./main.exe --pipe=[a socket/pipe file name given from vscode] \
     --socket=[a socket port number given from vscode]"
  in
  Arg.parse options (fun _ -> ()) usage_msg;
  let module Server =
    Controller.Rpc.Make (Interactor.Lsp.Make (Datastore.Document_store.Make)) in
  let sock_addr = Lwt_unix.ADDR_UNIX !socket_path in
  Lwt_io.with_connection sock_addr Server.start

let () = Lwt_main.run (main ())
