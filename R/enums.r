create_enum <- function(x) {
  setNames(as.list(x), x)
}

keywords <- create_enum(c(
  "AND", "CLASS", "ELSE", "FALSE", "FUN", "FOR", "IF", "NIL", "OR",
  "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE"
))

token_type <- create_enum(c(
  # single-character tokens
  "LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE", "COMMA", "DOT",
  "MINUS", "PLUS", "SEMICOLON", "SLASH", "STAR",

  # one or two character tokens
  "BANG", "BANG_EQUAL", "EQUAL", "EQUAL_EQUAL", "GREATER",
  "GREATER_EQUAL", "LESS", "LESS_EQUAL",

  # literals
  "IDENTIFIER", "STRING", "NUMBER",

  # keywords
  names(keywords),

  "EOF", "WHITESPACE", "COMMENT", "NEWLINE"
))

token_symbol <- list(
  # single character
  "LEFT_PAREN" = "(",
  "RIGHT_PAREN" = ")",
  "LEFT_BRACE" = "{",
  "RIGHT_BRACE" = "}",
  "COMMA" = ",",
  "DOT" = ".",
  "MINUS" = "-",
  "PLUS" = "+",
  "SEMICOLON" = ";",
  "SLASH" = "/",
  "STAR" = "*",
  # single or double character
  "BANG" = "!",
  "BANG_EQUAL" = "!=",
  "EQUAL" = "=",
  "EQUAL_EQUAL" = "==",
  "GREATER" = ">",
  "GREATER_EQUAL" = ">=",
  "LESS" = "<",
  "LESS_EQUAL" = "<=",
  # literals
  "IDENTIFIER" = "variable name",
  # keywords,
  "AND" = "and",
  "ELSE" = "else",
  "FALSE" = "false",
  "FOR" = "for",
  "FUN" = "fun",
  "IF" = "if",
  "NIL" = "nil",
  "OR" = "or",
  "PRINT" = "print",
  "RETURN" = "return",
  "TRUE" = "true",
  "VAR" = "var",
  "WHILE" = "while",
  # other
  "EOF" = ""
)
