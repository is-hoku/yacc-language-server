open Lwt.Syntax

module IO =
  Lsp.Io.Make
    (struct
      type 'a t = 'a Lwt.t

      let return = Lwt.return
      let raise = Lwt.fail

      module O = struct
        include Lwt.Syntax
      end
    end)
    (struct
      type input = Lwt_io.input_channel
      type output = Lwt_io.output_channel

      let read_line = Lwt_io.read_line_opt

      let read_exactly input len =
        let buffer = Bytes.create len in
        Lwt.catch
          (fun () ->
            let* () = Lwt_io.read_into_exactly input buffer 0 len in
            Lwt.return_some (Bytes.to_string buffer))
          (function End_of_file -> Lwt.return_none | exn -> raise exn)

      let write output lines =
        let* () =
          Lwt_list.iter_s (fun line -> Lwt_io.write output line) lines
        in
        Lwt_io.flush output
    end)
