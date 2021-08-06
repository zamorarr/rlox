#' Tokenize source code
#'
#' @param src character vector of lox source code
scan_tokens <- function(src) {
  stopifnot(is.character(src))
  stopifnot(length(src) == 1)

  s <- src
  tokens <- list()
  line <- 1L
  while (nchar(s) > 0) {
    # match token
    m <- match_token(s)
    if (m == -1) lox_error(line, sprintf("invalid syntax '%s'", s))

    # get lexeme
    lexeme <- regmatches(s, m)
    literal <- ""

    # determine type
    type <- switch(
      lexeme,
      "(" = token_type$LEFT_PAREN,
      ")" = token_type$RIGHT_PAREN,
      "{" = token_type$LEFT_BRACE,
      "}" = token_type$RIGHT_BRACE,
      "," = token_type$COMMA,
      "-" = token_type$MINUS,
      "+" = token_type$PLUS,
      ";" = token_type$SEMICOLON,
      "*" = token_type$STAR,
      "!" = token_type$BANG,
      "!=" = token_type$BANG_EQUAL,
      "=" = token_type$EQUAL,
      "==" = token_type$EQUAL_EQUAL,
      "<" = token_type$LESS,
      "<=" = token_type$LESS_EQUAL,
      ">" = token_type$GREATER,
      ">=" = token_type$GREATER_EQUAL,
      "/" = token_type$SLASH,
      " " = token_type$WHITESPACE,
      "\r" = token_type$WHITESPACE,
      "\t" = token_type$WHITESPACE,
      "\n" = {line <- line + 1L; token_type$WHITESPACE},
      NULL
      )

    if (is.null(type)) {
      if (substr(lexeme,1,1) == "\"") {
        type <- token_type$STRING
        literal <- substr(lexeme, 2, nchar(lexeme) - 1)
      } else if (substr(lexeme,1,1) %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
        type <- token_type$NUMBER
        literal <- lexeme
      } else if (substr(lexeme,1,2) == "//") {
        type <- token_type$COMMENT
      } else if (grepl("^[[:alnum:]_]+$", lexeme)) {
        if (lexeme %in% tolower(names(keywords))) {
          type <- toupper(lexeme)
        } else {
          type <- token_type$IDENTIFIER
        }
      }
    }

    # get new substring
    len <- attr(m, "match.length")
    current <- as.integer(m) + len
    s <- substr(s, current, nchar(s))

    # check if still null
    if (is.null(type)) {
      lox_error(line, sprintf("invalid token %s", lexeme))
    }

    # skip if whitespace
    if (type %in% c(token_type$WHITESPACE, token_type$COMMENT)) next

    # add to tokens
    tkn <- token(type, lexeme, literal, line)
    tokens <- c(tokens, list(tkn))
  }

  # add the end of file token
  tokens <- c(tokens,list(token(token_type$EOF, "", "", line)))

  # return
  tokens
}

match_token <- function(x) {
  pattern_single <- "[(){},.+;*-]"
  pattern_dual <- "(?:!=|==|>=|<=)"
  pattern_single_sometimes <- "[!=></]"
  pattern_whitespace <- "\\s+|\n|\r|\t"
  pattern_number <- "[0-9]+\\.?[0-9]*"
  pattern_comment <- "//[^\n]*(?:\n|$)"
  pattern_string <- "(\".*?\")"
  pattern_identifier <- "[[:alnum:]_]+"

  pattern <- paste(
    paste0("^", c(
    pattern_single, pattern_dual,
    pattern_number,
    pattern_comment, pattern_whitespace,
    pattern_string, pattern_identifier)),
    collapse = "|")

  #pattern <- sprintf("^%s", pattern)
  m <- regexpr(pattern, x, perl = TRUE)

  # special  check for sometimes single tokens
  if (m < 0) m <- regexpr(pattern_single_sometimes, x)

  # return match
  m
}
