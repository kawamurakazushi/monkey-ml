
let check expected got =
  if expected = got then Printf.sprintf "🎉 PASS - got %s" expected
  else Printf.sprintf "🔥 FAILED - expected %s, got %s" expected got

let test_let_statement expectedName (s: Ast.statement) =
  match s with
  | LetStatement ls ->
    if ls.name.value != expectedName
    then Printf.sprintf "??ls.name.value not %s got %s" expectedName ls.name.value
    else
      (
        match ls.name.token with
        | Token.Ident i ->
          if i != expectedName
          then Printf.sprintf "??ls.name.token not %s, got %s" expectedName i
          else "?? PASS"
        |_ -> ""
      )
  | ReturnStatement _ -> "??token is not `let`. got return statement"

let test_let_statements =
  let input  = "
let x = 5;
let y = 10;
let foobar = 828283;" in
  let program = Lexer.create input |> Parser.create |> Parser.parse in
  (
    if List.length program.statements != 3
    then Printf.sprintf "🔥 Statement does not container 3 statements got %d" @@ List.length program.statements
    else
      program.statements
      |> List.mapi (fun i -> test_let_statement (
          match i with
          | 0 -> "x"
          | 1 -> "y"
          | 2 -> "foobar"
          | _ -> "")
        )
      |> List.fold_left (^) ""
  )
  |> print_endline


let run_test =
  print_string "Testing PARSER";
  (* test_let_statement; *)
