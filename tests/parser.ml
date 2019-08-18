open Lib

let check expected got =
  if expected = got then Printf.sprintf "PASS - got %s" expected
  else Printf.sprintf "FAILED - expected %s, got %s" expected got

let test_let_statement2 expectedName (s: Ast.statement) =
  match s with
  | LetStatement ls ->
    if ls.name.value != expectedName
    then Printf.sprintf "ls.name.value not %s got %s" expectedName ls.name.value
    else
      (
        match ls.name.token with
        | Token.Ident i ->
          if i != expectedName
          then Printf.sprintf "ls.name.token not %s, got %s" expectedName i
          else "PASS"
        |_ -> ""
      )
  | ReturnStatement  -> "token is not `let`. got return statement"


let input = "
let x = 5;
let y = 10;
let foobar = 828283;"

let program = Lexer.create input |> Parser.create |> Parser.parse

let test_statements_length =
  let test () = Alcotest.(check int) "Statement length should be 3" 3 @@ List.length program.statements in
  Alcotest.test_case "Testing program's statement length" `Quick test


let test_is_let_statements =
  let test got () = Alcotest.(check bool) "Statement should be let statement" true got in
  program.statements
  |> List.map
    (fun statement ->
       Alcotest.test_case "Testing statement is LetStatement" `Quick @@ test (
         match statement with
         | Ast.LetStatement _ -> true
         | _ -> false
       ))

let test_let_statement_name expected (s: Ast.statement) =
  match s with
  | LetStatement ls ->
    let test () = Alcotest.(check string) "Test Let statement name" expected ls.name.value in
    Some (Alcotest.test_case (Printf.sprintf "The Let statement name is correct, expected=%s, got=%s" expected ls.name.value) `Quick test)
  | ReturnStatement  ->
    None

let test_let_statements_name =
  (if List.length program.statements = 3
   then List.map2 test_let_statement_name ["x"; "y"; "foobar"] program.statements
   else [])
  |> List.filter_map (fun x -> x)

let test_let_statement_name expectedToken (s: Ast.statement) =
  match s with
  | LetStatement ls ->
    let expected = Token.to_string expectedToken in
    let got = Token.to_string ls.name.token in
    let test () = Alcotest.(check string) "Test Let statement name" expected got in
    Some (Alcotest.test_case (Printf.sprintf "The Identifier Token is correct, expected=%s, got=%s" expected got) `Quick test)
  | ReturnStatement  ->
    None

let test_let_statements_token =
  (if List.length program.statements = 3
   then List.map2 test_let_statement_name [Token.Ident "x";Token.Ident  "y"; Token.Ident"foobar"] program.statements
   else [])
  |> List.filter_map (fun x -> x)


let tests = test_statements_length::[] @ test_is_let_statements @ test_let_statements_name @ test_let_statements_token
