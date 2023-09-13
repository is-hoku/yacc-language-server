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
        "a socket/pipe file name given from vscode" );
      ( "--socket",
        Arg.Set_int socket_port,
        "a socket port number given from vscode" );
    ]
  in
  let usage_msg =
    "Usage: ./main.exe --pipe=[a socket/pipe file name given from vscode] \
     --socket=[a socket port number given from vscode]"
  in
  Arg.parse options (fun _ -> ()) usage_msg;
  let sock_addr = Lwt_unix.ADDR_UNIX !socket_path in
  let module Server =
  Controller.Rpc.Make (Interactor.Lsp.Make (Datastore.Document_store.Make (Hashtbl
                                                                           .Make
                                                                             (struct
    type t = Datastore.Document_store.doc

    let equal = ( = )
    let hash = Hashtbl.hash
  end)))) in
  Lwt_io.with_connection sock_addr Server.start

(*
let () =
  Lwt_main.run
    (Lwt.join
       [
         client ();
         (let* () = Lwt_unix.sleep 3. in
          main ());
       ])
*)

let () = Lwt_main.run (main ())
