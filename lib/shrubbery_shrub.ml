open Prelude

open struct
  module Token_tree = Shrubbery_token_tree
  module Token = Shrubbery_token
end

type group = { items : item list }

and item =
  | Token of Token.t
  | Tree of
      { ldelim : Token.t
      ; groups : group list
      ; rdelim : Token.t
      }

and block =
  { colon : Token.t
  ; groups : group list
  }

and alt =
  { pipe : Token.t
  ; groups : group list
  }
[@@deriving sexp, equal, compare]

module Indexed = struct
  type group =
    { items : item list
    ; block : block option
    ; alts : alt list
    }

  and item =
    | Token of Token.ti
    | Tree of
        { ldelim : Token.ti
        ; groups : group list
        ; rdelim : Token.ti
        }

  and block =
    { colon : Token.ti
    ; groups : group list
    }

  and alt =
    { pipe : Token.ti
    ; groups : group list
    }
  [@@deriving sexp, equal, compare]
end
