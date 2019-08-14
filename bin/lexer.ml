type t = {
  input: string;
  position: int;
  readPosition: int;
  ch: char option;
}

let read l =
  {l with position = l.readPosition;
          readPosition = l.readPosition + 1;
          ch =
            if l.readPosition < String.length(l.input)
            then Some (String.get l.input l.readPosition)
            else None
  }

let create input =
  read {input = input;
        position = 0;
        readPosition = 0;
        ch = None}

let is_letter ch = match ch with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' -> true
  | _ -> false

let is_whitespace ch = match ch with
  | ' '
  | '\t' -> true
  | '\n' -> true
  | '\r' -> true
  | _ -> false

let is_digit ch = match ch with
  | '0' .. '9' -> true
  | _ -> false

let rec read_identifier l identifier =
  match l.ch with
  | Some c ->
    if is_letter c  then
      read_identifier (read l) (identifier ^ Printf.sprintf "%c" c)
    else
      (l, identifier)
  | None ->
    (l, identifier)

let rec read_number l digit =
  match l.ch with
  | Some c ->
    if is_digit c  then
      read_number (read l) (digit ^ Printf.sprintf "%c" c)
    else
      (l, digit)
  | None ->
    (l, digit)

let rec skip_whitespace l = match l.ch with
  | Some a  ->
    if is_whitespace a then
      skip_whitespace @@ read l
    else  l
  | None -> l

let next l =
  let clean_lexer = skip_whitespace l in
  let next_lexer = read clean_lexer in
  match clean_lexer.ch with
  | Some '=' ->
    (match next_lexer.ch with
     | Some peekCh ->
       if peekCh = '=' then
         (read next_lexer, Token.Eq)
       else
         (next_lexer, Token.Assign)
     | None -> (next_lexer, Token.Assign))
  | Some '+' -> (next_lexer, Token.Plus)
  | Some '-' -> (next_lexer, Token.Minus)
  | Some '!' ->
    (match next_lexer.ch with
     | Some peekCh ->
       if peekCh = '=' then
         (read next_lexer, Token.NotEq)
       else
         (next_lexer, Token.Bang)
     | None -> (next_lexer, Token.Assign))
  | Some '/' -> (next_lexer, Token.Slash)
  | Some '*' -> (next_lexer, Token.Asterisk)
  | Some '<' -> (next_lexer, Token.Lt)
  | Some '>' -> (next_lexer, Token.Gt)
  | Some ',' -> (next_lexer, Token.Comma)
  | Some ';' -> (next_lexer, Token.Semicolon)
  | Some '(' -> (next_lexer, Token.LParen)
  | Some ')' -> (next_lexer, Token.RParen)
  | Some '{' -> (next_lexer, Token.LBrace)
  | Some '}' -> (next_lexer, Token.RBrace)
  | Some a  ->
    if is_letter a then
      let (identfierLexer, identifier) = read_identifier next_lexer @@ Printf.sprintf "%c" a in
      (identfierLexer, Token.identfier_to_type identifier)
    else if is_digit a then
      let (numberLexer, number) =  read_number next_lexer @@ Printf.sprintf "%c" a in
      (numberLexer, Token.Int (int_of_string number))
    else
      (next_lexer, Token.Illegal)
  | None -> (next_lexer, Token.Eof)
