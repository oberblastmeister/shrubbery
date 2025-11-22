open Prelude

open struct
  module Token_tree = Shrubbery_token_tree
  module Token = Shrubbery_token
end

type group =
  { items : item list
  ; block : colon_block option
  ; alts : alt list
  }

and item =
  | Token of Token.t
  | Tree of
      { ldelim : Token.t
      ; groups : group_sep list
      ; rdelim : Token.t
      }

and colon_block =
  { token : Token.t
  ; block : block
  }

and block =
  { lbrace : Token.t
  ; groups : group_sep list
  ; rbrace : Token.t
  }

and group_sep =
  { group : group
  ; sep : Token.t option
  }

and alt =
  { pipe : Token.t
  ; groups : block
  }
[@@deriving sexp, equal, compare]

(* module Indexed = struct
  type group =
    { items : item list
    ; block : block option
    ; alts : alt list
    ; semi : Token.ti option
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
end *)
