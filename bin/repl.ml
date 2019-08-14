
let rec read l msg  = let (next_lexer, token) = Lexer.next l in
  if token = Token.Eof then msg ^ Token.to_string token
  else read next_lexer (msg ^ Token.to_string token ^ "\n")

let run_prompt = fun _ ->
  while true do
    print_string ">> ";
    let input = read_line () in
    let lexer = Lexer.create input in
    read lexer "" |> print_endline;
  done

let start =
  run_prompt ()
