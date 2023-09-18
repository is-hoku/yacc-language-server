module type Input = Repository.Document_store.S
module type Output = Usecase.Lsp.S

module Make (Repo : Input) : Output = struct
  module Initialize = Initialize.Make (Repo)
  module DidOpen = Didopen.Make (Repo)
  module DidChange = Didchange.Make (Repo)
  module Completion = Completion.Make (Repo)
end
