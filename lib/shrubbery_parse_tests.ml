open Prelude

open struct
  module Lexer = Shrubbery_lexer
  module Parser = Shrubbery_parser
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Delimit = Shrubbery_delimit
  module Layout = Shrubbery_layout
  module Shrub = Shrubbery_shrub
end

let check ?(remove_trivia = true) s =
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = Delimit.delimit tokens in
  let tts = Layout.insert_virtual_tokens tokens (Token_tree.root_to_indexed tts) in
  let tts = if remove_trivia then Token_tree.remove_trivia_root tts else tts in
  let block = Parser.parse tts in
  print_s [%sexp (block : Shrub.block)];
  if not (List.is_empty errors) then print_s [%sexp (errors : Delimit.Error.t list)];
  ()
;;

let%expect_test "basic" =
  check
    {|
def first:
  x
  y
  z
    |};
  [%expect
    {|
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;

let%expect_test "more" =
  check
    {|
def first:
  call_function(
    do:
      a
      b; c
      d,
    do:
      x; y;
      z,
    do:
      a; b; c; d
  )
  |};
  [%expect
    {|
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident call_function))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident do))))
                            (block
                             (((token Colon)
                               (block
                                ((lbrace VLBrace)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident a)))) (block ())
                                      (alts ())))
                                    (sep (VSemi)))
                                   ((group
                                     ((items ((Token (Ident b)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident c)))) (block ())
                                      (alts ())))
                                    (sep (VSemi)))
                                   ((group
                                     ((items ((Token (Ident d)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rbrace VRBrace))))))
                            (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident do))))
                            (block
                             (((token Colon)
                               (block
                                ((lbrace VLBrace)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident x)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident y)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group ((items ()) (block ()) (alts ())))
                                    (sep (VSemi)))
                                   ((group
                                     ((items ((Token (Ident z)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rbrace VRBrace))))))
                            (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident do))))
                            (block
                             (((token Colon)
                               (block
                                ((lbrace VLBrace)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident a)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident b)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident c)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident d)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rbrace VRBrace))))))
                            (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;

let%expect_test "invalid comma on toplevel" =
  check
    {|
def first:
  x; y; z
,
def another:
  x; y; z
    |};
  [%expect
    {|
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep (VSemi)))
       ((group ((items ((Token (Error ,)))) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;

let%expect_test "weird start indentation" =
  check
    {|
  def f:
    x; y; z
    
  def another:
    x
    y
  |};
  [%expect
    {|
    ((lbrace VLBrace)
     (groups
      (((group
         ((items ((Token (Ident def)) (Token (Ident f))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;

let%expect_test "weird start indentation dedented" =
  check
    {|
  def f:
    x; y; z
    
def another:
  x
  y
  
def g:
  x; y

  |};
  [%expect
    {|
    ((lbrace VLBrace)
     (groups
      (((group
         ((items ((Token (Ident def)) (Token (Ident f))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident g))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;
