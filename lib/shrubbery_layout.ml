open Prelude

open struct
  module Lexer = Shrubbery_lexer
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module To_tree = Shrubbery_to_tree
end

type context =
  | Unknown_indent
  | Known_indent of int
[@@deriving sexp, equal, compare]

module State = struct
  type t =
    { mutable contexts : context list
    ; mutable tts : Token_tree.t list
    ; top_level : bool
    }

  let create top_level = { contexts = []; tts = []; top_level }
  let finish st = List.rev st.tts

  let top_is st context =
    List.hd st.contexts
    |> Option.map ~f:(fun cx -> equal_context cx context)
    |> Option.value ~default:false
  ;;

  let add_context st context = st.contexts <- context :: st.contexts
  let start_block st = add_context st Unknown_indent

  let pop_context_exn st context =
    let hd, tl = List.hd_exn st.contexts, List.tl_exn st.contexts in
    assert (equal_context context hd);
    st.contexts <- tl
  ;;

  let add_tt st tt = st.tts <- tt :: st.tts

  let pop_all st =
    while not (List.is_empty st.contexts) do
      let hd, tl = List.hd_exn st.contexts, List.tl_exn st.contexts in
      if equal_context hd Unknown_indent then add_tt st (Token VLBrace);
      add_tt st (Token VRBrace);
      st.contexts <- tl
    done
  ;;

  let pop_offside ~add_semi st col =
    let rec loop () =
      match st.contexts with
      | [] -> ()
      | hd :: tl -> begin
        let pop_block () =
          st.contexts <- tl;
          add_tt st (Token VRBrace);
          loop ()
        in
        match hd with
        | Known_indent col' ->
          if col > col'
          then ()
          else if col = col'
          then begin
            if add_semi then add_tt st (Token VSemi)
          end
          else begin
            pop_block ()
          end
        | Unknown_indent -> begin
          add_tt st (Token VLBrace);
          match tl with
          | Unknown_indent :: _ -> assert false
          | Known_indent prev_col :: _ ->
            if col > prev_col
            then begin
              st.contexts <- Known_indent col :: tl
            end
            else begin
              pop_block ()
            end
          | [] -> st.contexts <- Known_indent col :: []
        end
      end
    in
    loop ()
  ;;
end

let insert_virtual_tokens tokens tts =
  let line_cols = Token.calculate_line_col tokens in
  let rec go_inner tts =
    let st = State.create false in
    go st tts;
    State.pop_all st;
    let res = State.finish st in
    res
  and go_root tts =
    let st = State.create true in
    State.add_context st Unknown_indent;
    go st tts;
    State.pop_all st;
    let res = State.finish st in
    res
  and go (st : State.t) (tts : Token_tree.Indexed.t list) =
    match tts with
    | [] -> ()
    | Token token :: tts' when Token.is_trivia token.token ->
      (* skip over trivia tokens *)
      State.add_tt st (Token token.token);
      go st tts'
    | Token token :: tts' -> begin
      let curr_lc = line_cols.(token.index) in
      (* commas end all blocks that were started in this token tree, but not at the top level, where there are no delimiters *)
      if Token.equal token.token Comma && not st.top_level
      then State.pop_all st
      else State.pop_offside st curr_lc.col ~add_semi:(not (Token.equal token.token Pipe));
      begin match token.token with
      | Colon | Pipe | Equal -> State.start_block st
      | _ -> ()
      end;
      State.add_tt st (Token token.token);
      go st tts'
    end
    | Tree { ldelim; tts = inner_tts; rdelim } :: tts' ->
      let curr_lc = line_cols.(ldelim.index) in
      let inner_tts = go_inner inner_tts in
      let tt =
        Token_tree.Tree { ldelim = ldelim.token; tts = inner_tts; rdelim = rdelim.token }
      in
      if Token.equal ldelim.token LBrace && State.top_is st Unknown_indent
      then begin
        State.pop_context_exn st Unknown_indent
      end
      else begin
        State.pop_offside st curr_lc.col ~add_semi:true
      end;
      State.add_tt st tt;
      go st tts'
  in
  go_root tts
;;

let check ?(remove_trivia = true) s =
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = To_tree.to_tree tokens in
  let tts = insert_virtual_tokens tokens (Token_tree.root_to_indexed tts) in
  let tts = if remove_trivia then Token_tree.remove_trivia_root tts else tts in
  print_s [%sexp (tts : Token_tree.t list)];
  if not (List.is_empty errors) then print_s [%sexp (errors : To_tree.Error.t list)];
  ()
;;

let%expect_test "basic" =
  check
    {|
def first:
  x
  y
  z
  
def weird:
{
  x;
  y; z;
}

def another:
  x
  y
  z
  
def d: x; y; z
       w
       l
    |};
  ();
  [%expect
    {|
    ((Token VLBrace) (Token (Ident def)) (Token (Ident first)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token VSemi) (Token (Ident y))
     (Token VSemi) (Token (Ident z)) (Token VRBrace) (Token VSemi)
     (Token (Ident def)) (Token (Ident weird)) (Token Colon)
     (Tree (ldelim LBrace)
      (tts
       ((Token (Ident x)) (Token Semi) (Token (Ident y)) (Token Semi)
        (Token (Ident z)) (Token Semi)))
      (rdelim RBrace))
     (Token VSemi) (Token (Ident def)) (Token (Ident another)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token VSemi) (Token (Ident y))
     (Token VSemi) (Token (Ident z)) (Token VRBrace) (Token VSemi)
     (Token (Ident def)) (Token (Ident d)) (Token Colon) (Token VLBrace)
     (Token (Ident x)) (Token Semi) (Token (Ident y)) (Token Semi)
     (Token (Ident z)) (Token VSemi) (Token (Ident w)) (Token VSemi)
     (Token (Ident l)) (Token VRBrace) (Token VRBrace))
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
    ((Token VLBrace) (Token (Ident def)) (Token (Ident first)) (Token Colon)
     (Token VLBrace) (Token (Ident call_function))
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
    ((Token VLBrace) (Token (Ident def)) (Token (Ident first)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token Semi) (Token (Ident y))
     (Token Semi) (Token (Ident z)) (Token VRBrace) (Token VSemi) (Token Comma)
     (Token VSemi) (Token (Ident def)) (Token (Ident another)) (Token Colon)
     (Token VLBrace) (Token (Ident x)) (Token Semi) (Token (Ident y))
     (Token Semi) (Token (Ident z)) (Token VRBrace) (Token VRBrace))
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
    ((Token VLBrace) (Token (Ident def)) (Token (Ident fib))
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
    ((Token VLBrace) (Token (Ident data)) (Token (Ident Option))
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
    ((Token VLBrace) (Token (Ident def)) (Token (Ident nested_match))
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
    ((Token VLBrace) (Token (Ident program)) (Token Colon) (Token VLBrace)
     (Token (Ident x)) (Token VSemi) (Token Semi) (Token (Ident y)) (Token VSemi)
     (Token (Ident z)) (Token VSemi) (Token Semi) (Token Semi) (Token Semi)
     (Token Semi) (Token Semi) (Token (Ident w)) (Token VRBrace) (Token VRBrace))
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
    ((Token VLBrace) (Token Colon) (Token VLBrace) (Token Colon) (Token VLBrace)
     (Token Colon) (Token VLBrace) (Token (Ident first)) (Token VSemi)
     (Token (Ident second)) (Token VSemi) (Token (Ident third)) (Token VRBrace)
     (Token VRBrace) (Token VRBrace) (Token VRBrace))
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
    ((Token VLBrace) (Token Colon) (Token VLBrace) (Token Colon) (Token VLBrace)
     (Token Colon) (Token VLBrace) (Token VRBrace) (Token VRBrace)
     (Token (Ident x)) (Token (Ident y)) (Token (Ident z)) (Token VRBrace)
     (Token VRBrace))
    |}]
;;

let%expect_test "start block with equal" =
  check {|
def first =
  x
  y
  z

def second = x; y
    |};
  [%expect {|
    ((Token VLBrace) (Token (Ident def)) (Token (Ident first)) (Token Equal)
     (Token VLBrace) (Token (Ident x)) (Token VSemi) (Token (Ident y))
     (Token VSemi) (Token (Ident z)) (Token VRBrace) (Token VSemi)
     (Token (Ident def)) (Token (Ident second)) (Token Equal) (Token VLBrace)
     (Token (Ident x)) (Token Semi) (Token (Ident y)) (Token VRBrace)
     (Token VRBrace))
    |}]
