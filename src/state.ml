open! Import
open Lsp.Types

type init = Uninitialized | Initialized of { params : InitializeParams.t }
type t = { init : init }

let create () = { init = Uninitialized }

let initialize_params state =
  match state.init with
  | Uninitialized -> None
  | Initialized init -> Some init.params

let initialize t params =
  match t.init with
  | Initialized _ -> None
  | Uninitialized -> Some { init = Initialized { params } }
