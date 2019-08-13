
let input  = "=+(){},;";;

let lexer = Lexer.create input

let run_test () =
  [Token.Assign;
   Token.Plus;
   Token.LParen;
   Token.RParen;
   Token.LBrace;
   Token.RBrace;
   Token.Comma;
   Token.Semicolon;
   Token.Eof]
  |> List.fold_left
    (fun (lexer, message) expectedToken->
       let (newLexer, token) = Lexer.next lexer
       in (newLexer, message ^ "\n" ^
                     if token = expectedToken then "🎉 PASS"
                     else Printf.sprintf "🔥 FAILED - expected %s, got %s" (Token.to_string expectedToken) (Token.to_string token)))
    (lexer , "")
  |> snd
  |> print_endline
