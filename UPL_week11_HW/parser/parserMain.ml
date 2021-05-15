let parse p =
  Parser.main Lexer.token (Lexing.from_string p)
