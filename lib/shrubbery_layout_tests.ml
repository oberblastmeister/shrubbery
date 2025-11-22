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
  print_s [%sexp (tts : Token_tree.t list)];
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
  ();
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident def)) (Token (Ident first))
     (Token Colon) (Token VLBrace) (Token (Ident x)) (Token VSemi)
     (Token (Ident y)) (Token VSemi) (Token (Ident z)) (Token VRBrace)
     (Token VRBrace))
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
    ((Token VLBrace) (Token VSemi) (Token (Ident def)) (Token (Ident first))
     (Token Colon) (Token VLBrace) (Token (Ident call_function))
     (Tree (ldelim LParen)
      (tts
       ((Token (Ident do)) (Token Colon) (Token VLBrace) (Token (Ident a))
        (Token VSemi) (Token (Ident b)) (Token Semi) (Token (Ident c))
        (Token VSemi) (Token (Ident d)) (Token VRBrace) (Token Comma)
        (Token (Ident do)) (Token Colon) (Token VLBrace) (Token (Ident x))
        (Token Semi) (Token (Ident y)) (Token Semi) (Token VSemi)
        (Token (Ident z)) (Token VRBrace) (Token Comma) (Token (Ident do))
        (Token Colon) (Token VLBrace) (Token (Ident a)) (Token Semi)
        (Token (Ident b)) (Token Semi) (Token (Ident c)) (Token Semi)
        (Token (Ident d)) (Token VRBrace)))
      (rdelim RParen))
     (Token VRBrace) (Token VRBrace))
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
    ((Token VLBrace) (Token VSemi) (Token (Ident def)) (Token (Ident first))
     (Token Colon) (Token VLBrace) (Token (Ident x)) (Token Semi)
     (Token (Ident y)) (Token Semi) (Token (Ident z)) (Token VRBrace)
     (Token VSemi) (Token (Error ,)) (Token VSemi) (Token (Ident def))
     (Token (Ident another)) (Token Colon) (Token VLBrace) (Token (Ident x))
     (Token Semi) (Token (Ident y)) (Token Semi) (Token (Ident z))
     (Token VRBrace) (Token VRBrace))
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
    ((Token VLBrace) (Token (Ident def)) (Token (Ident f)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token Semi) (Token (Ident y))
     (Token Semi) (Token (Ident z)) (Token VRBrace) (Token VSemi)
     (Token (Ident def)) (Token (Ident another)) (Token Colon) (Token VLBrace)
     (Token (Ident x)) (Token VSemi) (Token (Ident y)) (Token VRBrace)
     (Token VSemi) (Token (Ident def)) (Token (Ident g)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token Semi) (Token (Ident y))
     (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "fib" =
  check
    {|
    
def fib pos_int
| fib(0): 1
| fib(1): 1
| fib(n nat, n bool): fib(n - 1) + fib(n - 2)

def another:
  (x, y, z)

    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident def)) (Token (Ident fib))
     (Token (Ident pos_int)) (Token Pipe) (Token VLBrace) (Token (Ident fib))
     (Tree (ldelim LParen) (tts ((Token (Number 0)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token (Number 1)) (Token VRBrace)
     (Token VRBrace) (Token Pipe) (Token VLBrace) (Token (Ident fib))
     (Tree (ldelim LParen) (tts ((Token (Number 1)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token (Number 1)) (Token VRBrace)
     (Token VRBrace) (Token Pipe) (Token VLBrace) (Token (Ident fib))
     (Tree (ldelim LParen)
      (tts
       ((Token (Ident n)) (Token (Ident nat)) (Token Comma) (Token (Ident n))
        (Token (Ident bool))))
      (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token (Ident fib))
     (Tree (ldelim LParen)
      (tts ((Token (Ident n)) (Token (Operator -)) (Token (Number 1))))
      (rdelim RParen))
     (Token (Operator +)) (Token (Ident fib))
     (Tree (ldelim LParen)
      (tts ((Token (Ident n)) (Token (Operator -)) (Token (Number 2))))
      (rdelim RParen))
     (Token VRBrace) (Token VRBrace) (Token VSemi) (Token (Ident def))
     (Token (Ident another)) (Token Colon) (Token VLBrace)
     (Tree (ldelim LParen)
      (tts
       ((Token (Ident x)) (Token Comma) (Token (Ident y)) (Token Comma)
        (Token (Ident z))))
      (rdelim RParen))
     (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "semi after alternatives" =
  check
    {|
data Option(a):
| Some(a)
| None
;

data Either(a, b):
| Left(a)
| Right(b)

record Pair(a, b):
  fst a
  snd b

    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident data)) (Token (Ident Option))
     (Tree (ldelim LParen) (tts ((Token (Ident a)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token VRBrace) (Token Pipe) (Token VLBrace)
     (Token (Ident Some))
     (Tree (ldelim LParen) (tts ((Token (Ident a)))) (rdelim RParen))
     (Token VRBrace) (Token Pipe) (Token VLBrace) (Token (Ident None))
     (Token VRBrace) (Token VSemi) (Token Semi) (Token VSemi)
     (Token (Ident data)) (Token (Ident Either))
     (Tree (ldelim LParen)
      (tts ((Token (Ident a)) (Token Comma) (Token (Ident b)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token VRBrace) (Token Pipe) (Token VLBrace)
     (Token (Ident Left))
     (Tree (ldelim LParen) (tts ((Token (Ident a)))) (rdelim RParen))
     (Token VRBrace) (Token Pipe) (Token VLBrace) (Token (Ident Right))
     (Tree (ldelim LParen) (tts ((Token (Ident b)))) (rdelim RParen))
     (Token VRBrace) (Token VSemi) (Token (Ident record)) (Token (Ident Pair))
     (Tree (ldelim LParen)
      (tts ((Token (Ident a)) (Token Comma) (Token (Ident b)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token (Ident fst)) (Token (Ident a))
     (Token VSemi) (Token (Ident snd)) (Token (Ident b)) (Token VRBrace)
     (Token VRBrace))
    |}]
;;

let%expect_test "nested match" =
  check
    {|
def nested_match(x, y):
  match x:
  | Some(x):
    match y:
    | Some(y): print(x, y)
    | None: throw()
  | None:
    throw()

  |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident def))
     (Token (Ident nested_match))
     (Tree (ldelim LParen)
      (tts ((Token (Ident x)) (Token Comma) (Token (Ident y)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token (Ident match)) (Token (Ident x))
     (Token Colon) (Token VLBrace) (Token VRBrace) (Token Pipe) (Token VLBrace)
     (Token (Ident Some))
     (Tree (ldelim LParen) (tts ((Token (Ident x)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token VRBrace) (Token VSemi)
     (Token (Ident match)) (Token (Ident y)) (Token Colon) (Token VLBrace)
     (Token VRBrace) (Token Pipe) (Token VLBrace) (Token (Ident Some))
     (Tree (ldelim LParen) (tts ((Token (Ident y)))) (rdelim RParen))
     (Token Colon) (Token VLBrace) (Token (Ident print))
     (Tree (ldelim LParen)
      (tts ((Token (Ident x)) (Token Comma) (Token (Ident y)))) (rdelim RParen))
     (Token VRBrace) (Token VRBrace) (Token Pipe) (Token VLBrace)
     (Token (Ident None)) (Token Colon) (Token VLBrace) (Token (Ident throw))
     (Tree (ldelim LParen) (tts ()) (rdelim RParen)) (Token VRBrace)
     (Token VRBrace) (Token VRBrace) (Token Pipe) (Token VLBrace)
     (Token (Ident None)) (Token Colon) (Token VLBrace) (Token VRBrace)
     (Token VSemi) (Token (Ident throw))
     (Tree (ldelim LParen) (tts ()) (rdelim RParen)) (Token VRBrace)
     (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "weird semi" =
  check
    {|
program:
  x
  ; y
  z
  ; ; ; ; ; w
    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident program)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token VSemi) (Token Semi)
     (Token (Ident y)) (Token VSemi) (Token (Ident z)) (Token VSemi) (Token Semi)
     (Token Semi) (Token Semi) (Token Semi) (Token Semi) (Token (Ident w))
     (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "weird empty blocks" =
  check
    {|
: : :
                 first
                 second
                 third
    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token Colon) (Token VLBrace) (Token Colon)
     (Token VLBrace) (Token Colon) (Token VLBrace) (Token (Ident first))
     (Token VSemi) (Token (Ident second)) (Token VSemi) (Token (Ident third))
     (Token VRBrace) (Token VRBrace) (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "weird empty blocks find first token" =
  check
    {|
 : :    : 
        x
        y
        z
    |};
  [%expect
    {|
    ((Token VLBrace) (Token Colon) (Token VLBrace) (Token Colon) (Token VLBrace)
     (Token Colon) (Token VLBrace) (Token VRBrace) (Token VSemi)
     (Token (Ident x)) (Token VSemi) (Token (Ident y)) (Token VSemi)
     (Token (Ident z)) (Token VRBrace) (Token VRBrace) (Token VRBrace))
    |}];
  check
    {|
: :    :
      x
      y
      z
    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token Colon) (Token VLBrace) (Token Colon)
     (Token VLBrace) (Token Colon) (Token VLBrace) (Token VRBrace)
     (Token VRBrace) (Token (Ident x)) (Token (Ident y)) (Token (Ident z))
     (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "start block with equal" =
  check
    {|
def first =
  x
  y
  z

def second = x; y
    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident def)) (Token (Ident first))
     (Token Equal) (Token VLBrace) (Token (Ident x)) (Token VSemi)
     (Token (Ident y)) (Token VSemi) (Token (Ident z)) (Token VRBrace)
     (Token VSemi) (Token (Ident def)) (Token (Ident second)) (Token Equal)
     (Token VLBrace) (Token (Ident x)) (Token Semi) (Token (Ident y))
     (Token VRBrace) (Token VRBrace))
    |}]
;;

let%expect_test "unnecessary semicolons" =
  check
    {|
def first: 
  x;
  y;
  z
    |};
  [%expect
    {|
    ((Token VLBrace) (Token VSemi) (Token (Ident def)) (Token (Ident first))
     (Token Colon) (Token VLBrace) (Token (Ident x)) (Token Semi) (Token VSemi)
     (Token (Ident y)) (Token Semi) (Token VSemi) (Token (Ident z))
     (Token VRBrace) (Token VRBrace))
    |}]
;;
