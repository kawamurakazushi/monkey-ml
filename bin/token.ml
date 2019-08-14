type token =
  | Illegal
  | Eof
  | Ident of string
  | Int of int
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let

let to_string t = match t with
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  | Ident str -> Printf.sprintf "IDENT(%s)" str
  | Int int -> Printf.sprintf "INT(%d)" int
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  | Function -> "FUNCTION"
  | Let -> "LET"

let identfier_to_type a = match a with
  | "let" -> Let
  | "fn" -> Function
  | str -> Ident str