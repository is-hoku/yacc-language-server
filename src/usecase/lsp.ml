module type S = sig
  module Initialize : Initialize.S
  module Textdocument : Textdocument.S
end
