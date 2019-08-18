type token =
  | Illegal
  | Eof
  | Ident of string
  | Int of int
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Lt
  | Gt
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  | Eq
  | NotEq

let to_string t = match t with
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  | Ident str -> Printf.sprintf "IDENT(%s)" str
  | Int int -> Printf.sprintf "INT(%d)" int
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Minus -> "MINUS"
  | Bang -> "BANG"
  | Asterisk -> "ASTERISK"
  | Slash -> "SLASH"
  | Lt -> "LT"
  | Gt -> "GT"
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  | Function -> "FUNCTION"
  | Let -> "LET"
  | True -> "TRUE"
  | False -> "FALSE"
  | If -> "IF"
  | Else -> "ELSE"
  | Return -> "RETURN"
  | Eq -> "EQ"
  | NotEq -> "NOT_EQ"

let identfier_to_type a = match a with
  | "let" -> Let
  | "fn" -> Function
  | "true" -> True
  | "false" -> False
  | "if" ->  If
  | "else" ->  Else
  | "return" ->  Return
  | str -> Ident str