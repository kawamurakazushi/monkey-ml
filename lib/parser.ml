type parser = {
  lexer: Lexer.lexer;
  cur_token: Token.token;
  peek_token: Token.token;
}

let create l =
  let (cur_lexer, cur_token)  = Lexer.next l in
  let (peek_lexer , peek_token)  = Lexer.next cur_lexer in {
    lexer = peek_lexer;
    cur_token = cur_token;
    peek_token = peek_token;
  }

let next_token parser =
  let (next_lexer, next_token) = Lexer.next parser.lexer in {
    lexer = next_lexer;
    cur_token = parser.peek_token;
    peek_token = next_token;
  }

(* Returns parser *)
let expect_peek parser token =
  if parser.peek_token = token then Some (next_token parser) else None

(* returns a new parser, and the let statement *)
let parse_let_statement parser =
  match parser.peek_token with
  | Ident ident ->
    (* let next_parser = next_token parser in *)
    Some (Ast.LetStatement
            {token = Token.Let;
             name = {
               token = Ident ident;
               value = ident;
             };
             value = {
               node = {
                 token= Token.Let;
               };
               token = Token.Let}
            })
  | _ -> None

(* rtoooeturns list of statements *)
let rec parse_statement parser (statements: Ast.statement list ) =
  let _ = parser.cur_token |> Token.to_string |> print_endline in
  match parser.cur_token with
  | Token.Eof -> statements
  | Token.Let ->
    let next_parser = next_token parser in
    (parse_let_statement parser
     |> Option.map (fun a -> [a])
     |> Option.value ~default:[])
    @ parse_statement next_parser statements
  | _ ->
    let next_parser = next_token parser in
    parse_statement next_parser statements


let parse parser =
  (* let program =  { Ast.statements = [] } in *)
  let statements = parse_statement parser [] in
  {Ast.statements = statements}