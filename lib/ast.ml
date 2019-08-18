type node = {token: Token.token}

type identifier = {token: Token.token; value: string}

type expression = {node: node; token: Token.token;}

(* Let Statement *)

type let_statement =
  {token: Token.token;
   name: identifier;
   value: expression}

type return_statement = {value: expression}


let statement_node ls = ls

let token ls = ls.token

(* Use Extensible Record if it's possible
   type t = {node: node; token: Token.token;} *)

type statement = LetStatement of let_statement | ReturnStatement

type program = {statements : statement list}
