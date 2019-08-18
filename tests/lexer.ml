open Lib

let check name lexer a =
  a |> List.fold_left
    (fun (lexer, tests) expected_token ->
       let (new_lexer, token) = Lexer.next lexer in
       let expected = expected_token |> Token.to_string in
       let got = token |> Token.to_string in
       let test ()  = Alcotest.(check string) "same token" expected got in
       (new_lexer,
        tests @
        [(Alcotest.test_case
            (Printf.sprintf "%s expected=%s, got=%s" name expected got) `Quick test
         )]
       )
    )
    (lexer , [])
  |> snd

let test_basic =
  let input  = "=+(){},;" in
  let lexer = Lexer.create input; in
  [Token.Assign;
   Token.Plus;
   Token.LParen;
   Token.RParen;
   Token.LBrace;
   Token.RBrace;
   Token.Comma;
   Token.Semicolon;
   Token.Eof]
  |> check "Basic" lexer

let test_advance  =
  let input  = "let five = 5;
  let ten = 10;

  let add = fn(x, y) {
    x + y;
  }

  let result = add(five, ten);

  !-/*5;
  5 < 10 > 5;

  if (5 < 10) {
    return true;
  } else {
    return false;
  }

  10 == 10;
  10 != 9;
  " in
  let lexer = Lexer.create input; in
  [Token.Let;
   Token.Ident "five";
   Token.Assign;
   Token.Int 5;
   Token.Semicolon;
   Token.Let;
   Token.Ident "ten";
   Token.Assign;
   Token.Int 10;
   Token.Semicolon;
   Token.Let;
   Token.Ident "add";
   Token.Assign;
   Token.Function;
   Token.LParen;
   Token.Ident "x";
   Token.Comma;
   Token.Ident "y";
   Token.RParen;
   Token.LBrace;
   Token.Ident "x";
   Token.Plus;
   Token.Ident "y";
   Token.Semicolon;
   Token.RBrace;
   Token.Let;
   Token.Ident "result";
   Token.Assign;
   Token.Ident "add";
   Token.LParen;
   Token.Ident "five";
   Token.Comma;
   Token.Ident "ten";
   Token.RParen;
   Token.Semicolon;
   Token.Bang;
   Token.Minus;
   Token.Slash;
   Token.Asterisk;
   Token.Int 5;
   Token.Semicolon;
   Token.Int 5;
   Token.Lt;
   Token.Int 10;
   Token.Gt;
   Token.Int 5;
   Token.Semicolon;
   Token.If;
   Token.LParen;
   Token.Int 5;
   Token.Lt;
   Token.Int 10;
   Token.RParen;
   Token.LBrace;
   Token.Return;
   Token.True;
   Token.Semicolon;
   Token.RBrace;
   Token.Else;
   Token.LBrace;
   Token.Return;
   Token.False;
   Token.Semicolon;
   Token.RBrace;
   Token.Int 10;
   Token.Eq;
   Token.Int 10;
   Token.Semicolon;
   Token.Int 10;
   Token.NotEq;
   Token.Int 9;
   Token.Semicolon;
   Token.Eof;
  ]
  |> check "Advance" lexer

let tests =  test_basic @ test_advance
