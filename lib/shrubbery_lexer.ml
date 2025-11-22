open Prelude
module Token = Shrubbery_token

let rec take_while ~f i s =
  if i >= String.length s || not (f s.[i]) then i else take_while ~f (i + 1) s
;;

let is_operator_char c =
  match c with
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '<' | '@' | '^' | '|' -> true
  | _ -> false
;;

let is_ident_start c = Char.is_alpha c || Char.equal c '_'

let is_ident_continue c =
  is_ident_start c || Char.is_digit c || Char.equal c '\'' || Char.equal c '?'
;;

let is_number_start c = Char.is_digit c

let is_number_continue c =
  is_number_start c || Char.equal c '_' || Char.is_alpha c || Char.equal c '.'
;;

let lex s =
  let rec go (acc : Token.t list) (i : int) : Token.t list =
    if i >= String.length s
    then finish acc
    else begin
      match s.[i] with
      | '(' -> go (Token.LParen :: acc) (i + 1)
      | ')' -> go (Token.RParen :: acc) (i + 1)
      | '[' -> go (Token.LBrack :: acc) (i + 1)
      | ']' -> go (Token.RBrack :: acc) (i + 1)
      | '{' -> go (Token.LBrace :: acc) (i + 1)
      | '}' -> go (Token.RBrace :: acc) (i + 1)
      | ',' -> go (Token.Comma :: acc) (i + 1)
      | ':' -> go (Token.Colon :: acc) (i + 1)
      | ';' -> go (Token.Semi :: acc) (i + 1)
      | '.' -> go (Token.Dot :: acc) (i + 1)
      | '"' -> go_string acc i (i + 1)
      | '=' when i + 1 >= String.length s || not (is_operator_char s.[i + 1]) ->
        go (Token.Equal :: acc) (i + 1)
      | '|' when i + 1 >= String.length s || not (is_operator_char s.[i + 1]) ->
        go (Token.Pipe :: acc) (i + 1)
      | ' ' -> go_whitespace acc i (i + 1)
      | '\n' -> go (Token.Newline :: acc) (i + 1)
      | '/' -> go_comment acc i (i + 1)
      | '~' -> go_keyword acc i (i + 1)
      | _ when is_number_start s.[i] -> go_number acc i (i + 1)
      | _ when is_ident_start s.[i] -> go_ident acc i (i + 1)
      | _ when is_operator_char s.[i] -> go_operator acc i (i + 1)
      | c -> go (Token.Error (String.of_char c) :: acc) (i + 1)
    end
  and go_number acc start i =
    let i = take_while ~f:is_number_continue i s in
    go (Token.Number (String.sub s ~pos:start ~len:(i - start)) :: acc) i
  and go_string acc start i =
    let i = take_while ~f:(fun c -> not (Char.equal c '"' || Char.equal c '\n')) i s in
    let content = String.sub s ~pos:(start + 1) ~len:(i - (start + 1)) in
    if i >= String.length s || Char.equal s.[i] '\n'
    then go (Error content :: acc) i
    else go (String content :: acc) (i + 1)
  and go_ident acc start i =
    let i = take_while ~f:is_ident_continue i s in
    go (Token.Ident (String.sub s ~pos:start ~len:(i - start)) :: acc) i
  and go_keyword acc start i =
    if i >= String.length s || not (is_ident_start s.[i])
    then go (Token.Error (String.of_char s.[i]) :: acc) i
    else begin
      let i = i + 1 in
      let i = take_while ~f:is_ident_continue i s in
      go (Token.Keyword (String.sub s ~pos:(start + 1) ~len:(i - (start + 1))) :: acc) i
    end
  and go_operator acc start i =
    let i = take_while ~f:is_operator_char i s in
    go (Token.Operator (String.sub s ~pos:start ~len:(i - start)) :: acc) i
  and go_comment acc start i =
    if i >= String.length s || not (Char.equal s.[i] '/')
    then go_operator acc start i
    else begin
      let i = i + 1 in
      let i = take_while ~f:(fun c -> not (Char.equal c '\n')) i s in
      go (Token.Comment (String.sub s ~pos:(start + 2) ~len:(i - (start + 2))) :: acc) i
    end
  and go_whitespace acc start i =
    let i = take_while ~f:(Char.equal ' ') i s in
    go (Token.Whitespace (i - start) :: acc) i
  and finish acc = List.rev acc in
  go [] 0
;;

let%expect_test "smoke" =
  let check s =
    let res = lex s in
    print_s [%sexp (res : Token.t list)]
  in
  check
    {|
+ / + == awefpoiu'aewf? "aewf"first.second call_function(arg1, arg2) ~first ~second ~else
// awefaewfaewfawef
1.23__1.3_4.3
// another
    |};
  [%expect
    {|
    (Newline (Operator +) (Whitespace 1) (Operator /) (Whitespace 1) (Operator +)
     (Whitespace 1) (Operator ==) (Whitespace 1) (Ident awefpoiu'aewf?)
     (Whitespace 1) (String aewf) (Ident first) Dot (Ident second) (Whitespace 1)
     (Ident call_function) LParen (Ident arg1) Comma (Whitespace 1) (Ident arg2)
     RParen (Whitespace 1) (Keyword first) (Whitespace 1) (Keyword second)
     (Whitespace 1) (Keyword else) Newline (Comment " awefaewfaewfawef") Newline
     (Number 1.23__1.3_4.3) Newline (Comment " another") Newline (Whitespace 4))
    |}]
;;
