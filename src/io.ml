open Lwt

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

      let read_line input =
        Lwt.catch
          (fun () -> Lwt_io.read_line_opt input)
          (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)

      let read_exactly input len =
        let buffer = Bytes.create len in
        Lwt.catch
          (fun () ->
            Lwt_io.read_into_exactly input buffer 0 len >>= fun () ->
            Lwt.return (Some (Bytes.to_string buffer)))
          (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)

      let write output line =
        Lwt_io.write_line output line >>= fun () -> Lwt_io.flush output
    end)
