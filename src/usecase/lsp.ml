module type S = sig
  module Initialize : Initialize.S
  module DidOpen : Didopen.S
  module DidChange : Didchange.S
  module DidSave : Didsave.S
  module Completion : Completion.S
end
