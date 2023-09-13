open Io
open Lwt.Syntax

module Lwt_io_run = struct
  type 'a t = 'a Lwt.t
end

module Lwt_io_flush = struct
  type 'a t = 'a Lwt.t

  let return x = Lwt.return x
  let bind x ~f = Lwt.bind x f
  let to_run x = x
end

module Expect_test_config :
  Expect_test_config_types.S
    with module IO_run = Lwt_io_run
     and module IO_flush = Lwt_io_flush = struct
  module IO_run = Lwt_io_run
  module IO_flush = Lwt_io_flush

  let run x = Lwt_main.run (x ())
  let flushed () = Lwt_io.(buffered stdout = 0)
  let sanitize x = String.map (function '\r' -> ' ' | c -> c) x
  let upon_unreleasable_issue = `CR
end

let%expect_test "stdout" =
  let output = Lwt_io.stdout in
  IO.write output
    (Jsonrpc.Packet.t_of_yojson
       (`Assoc
         [
           ("jsonrpc", `String "2.0");
           ("method", `String "Test JSON-RPC");
           ( "params",
             `Assoc [ ("test_int", `Int 0123); ("test_string", `String "test") ]
           );
           ("id", `Int 123);
         ]))
  |> Lwt_main.run;
  [%expect
    {|
    Content-Length: 98
    Content-Type: application/vscode-jsonrpc; charset=utf-8

    {"id":123,"params":{"test_int":123,"test_string":"test"},"method":"Test JSON-RPC","jsonrpc":"2.0"}|}]

let%expect_test "stdin" =
  let main () =
    let input =
      Lwt_io.of_bytes ~mode:Lwt_io.Input
        (Lwt_bytes.of_string
           {|Content-Length: 116

{"jsonrpc": "2.0", "id": 1, "method": "textDocument/completion", "params": {"test_int": 123, "test_string": "test"}}|})
    in
    let output = Lwt_io.stdout in
    let* packet = IO.read input in
    match packet with
    | None -> Lwt.return_unit
    | Some json -> IO.write output json
  in
  Lwt_main.run (main ());
  [%expect
    {|
    Content-Length: 106
    Content-Type: application/vscode-jsonrpc; charset=utf-8

    {"id":1,"params":{"test_int":123,"test_string":"test"},"method":"textDocument/completion","jsonrpc":"2.0"}|}]
