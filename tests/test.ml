
let test_suites: unit Alcotest.test list = [
  "Testing Lexer", Lexer.tests;
  "Testing Parser", Parser.tests;
]

let () = Alcotest.run "monkey-ml" test_suites