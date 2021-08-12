scan_tokens <- function(x) {
  line <- 1L
  tokens <- list()
  while(nchar(x) > 0) {
    s <- scan_token(x, line)
    token <- s$token
    x <- s$x
    line <- token$line

    # skip token if it is whitespace or comment
    if (!token$type %in% c(token_type$WHITESPACE, token_type$NEWLINE, token_type$COMMENT)) {
      tokens <- c(tokens, list(token))
    }
  }

  # add EOF token
  tokens <- c(tokens, list(token(token_type$EOF, "", "", line)))
  tokens
}

scan_token <- function(x, line = 1L) {

  literal <- ""
  if (m <- match_left_paren(x)) {
    type <- token_type$LEFT_PAREN
  } else if (m <- match_right_paren(x)) {
    type <- token_type$RIGHT_PAREN
  } else if (m <- match_left_brace(x)) {
    type <- token_type$LEFT_BRACE
  } else if (m <- match_right_brace(x)) {
    type <- token_type$RIGHT_BRACE
  } else if (m <- match_comma(x)) {
    type <- token_type$COMMA
  } else if (m <- match_minus(x)) {
    type <- token_type$MINUS
  } else if (m <- match_plus(x)) {
    type <- token_type$PLUS
  } else if (m <- match_semicolon(x)) {
    type <- token_type$SEMICOLON
  } else if (m <- match_star(x)) {
    type <- token_type$STAR
  } else if (m <- match_bang_equal(x)) {
    type <- token_type$BANG_EQUAL
  } else if (m <- match_bang(x)) {
    type <- token_type$BANG
  } else if (m <- match_equal_equal(x)) {
    type <- token_type$EQUAL_EQUAL
  } else if (m <- match_equal(x)) {
    type <- token_type$EQUAL
  } else if (m <- match_less_equal(x)) {
    type <- token_type$LESS_EQUAL
  } else if (m <- match_less(x)) {
    type <- token_type$LESS
  } else if (m <- match_greater_equal(x)) {
    type <- token_type$GREATER_EQUAL
  } else if (m <- match_greater(x)) {
    type <- token_type$GREATER
  } else if (m <- match_comment(x)) {
    type <- token_type$COMMENT
  } else if (m <- match_slash(x)) {
    type <- token_type$SLASH
  } else if (m <- match_whitespace(x)) {
    type <- token_type$WHITESPACE
  } else if (m <- match_newline(x)) {
    type <- token_type$NEWLINE
    line <- line + 1L
  } else if (m <- match_number(x)) {
    type <- token_type$NUMBER
    literal <- as.double(regmatches(x, m))
  } else if (m <- match_identifier(x)) {
    lexeme <- regmatches(x, m)
    if (toupper(lexeme) %in% names(keywords)) {
      type <- toupper(lexeme)
    } else {
      type <- token_type$IDENTIFIER
    }
  } else if (m <- match_string(x)) {
    type <- token_type$STRING
    literal <- as.character(regmatches(x, m))
    literal <- substr(literal, 2, nchar(literal) - 1)
  } else {
    lox_scanner_error(sprintf("Unexpected character `%s`", substr(x, 1, 1)), line)
  }

  # create token
  lexeme <- regmatches(x, m)
  tkn <- token(type, lexeme, literal, line)

  # update string
  len <- attr(m, "match.length")
  current <- as.integer(m) + len
  x <- substr(x, current, nchar(x))

  # return
  list(token = tkn, x = x)
}
