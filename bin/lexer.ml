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

let next l = let token = match l.ch with
    | Some '=' -> Token.Assign
    | Some '+' -> Token.Plus
    | Some ',' -> Token.Comma
    | Some ';' -> Token.Semicolon
    | Some '(' -> Token.LParen
    | Some ')' -> Token.RParen
    | Some '{' -> Token.LBrace
    | Some '}' -> Token.RBrace
    | _ -> Token.Eof

  in
  (read l, token)
