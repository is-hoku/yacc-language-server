module type Input = Repository.Document_store.S
module type Output = Usecase.Lsp.S

module Make (Repo : Input) : Output = struct
  module Initialize = Initialize.Make (Repo)
  module Textdocument = Textdocument.Make (Repo)
end
