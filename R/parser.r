parse_lox <- function(tokens) {
  # initialize output
  statements <- list()

  i <- 1L
  while(length(tokens) > 1) {
    p <- parse_statement(tokens)
    statements[[i]] <- p$stmt
    tokens <- p$tokens
    i <- i + 1L
  }

  # return output
  statements
}

parse_statement <- function(tokens) {
  # check for unary operators
  token <- tokens[[1]]
  if (token$type == token_type$PRINT) {
    return(parse_stmt_print(tokens[-1]))
  }

  # parse as primary expression
  parse_stmt_expression(tokens)
}

parse_stmt_print <- function(tokens) {
  p <- parse_expression(tokens)
  expr <- p$expr
  tokens <- p$tokens

  tokens <- consume(tokens, token_type$SEMICOLON)
  list(stmt = stmt_print(expr), tokens = tokens)
}

parse_stmt_expression <- function(tokens) {
  p <- parse_expression(tokens)
  expr <- p$expr
  tokens <- p$tokens

  tokens <- consume(tokens, token_type$SEMICOLON)
  list(stmt = stmt_expression(expr), tokens = tokens)
}

parse_expression <- function(tokens) {
  #cat("parse_expression\n")
  parse_equality(tokens)
}

parse_equality <- function(tokens) {
  #cat("parse_equality\n")
  left <- parse_comparison(tokens)
  expr <- left$expr
  tokens <- left$tokens

  while(tokens[[1]]$type %in% c(token_type$BANG_EQUAL, token_type$EQUAL_EQUAL)) {
    operator <- tokens[[1]]
    right <- parse_term(tokens[-1])

    expr <- expr_binary(expr, operator, right$expr)
    tokens <- right$tokens
  }

  # return
  list(expr = expr, tokens = tokens)
}

parse_comparison <- function(tokens) {
  #cat("parse_comparison\n")
  left <- parse_term(tokens)
  expr <- left$expr
  tokens <- left$tokens

  while(tokens[[1]]$type %in% c(token_type$GREATER, token_type$GREATER_EQUAL, token_type$LESS, token_type$LESS_EQUAL)) {
    operator <- tokens[[1]]
    right <- parse_term(tokens[-1])

    expr <- expr_binary(expr, operator, right$expr)
    tokens <- right$tokens
  }

  # return
  list(expr = expr, tokens = tokens)
}

parse_term <- function(tokens) {
  #cat("parse_term\n")
  left <- parse_factor(tokens)
  expr <- left$expr
  tokens <- left$tokens

  # loop
  while(tokens[[1]]$type %in% c(token_type$MINUS, token_type$PLUS)) {
    operator <- tokens[[1]]
    right <- parse_factor(tokens[-1])

    expr <- expr_binary(expr, operator, right$expr)
    tokens <- right$tokens
  }

  # return
  list(expr = expr, tokens = tokens)
}

parse_factor <- function(tokens) {
  #cat("parse_factor\n")
  left <- parse_unary(tokens)
  expr <- left$expr
  tokens <- left$tokens

  # get next token
  while (tokens[[1]]$type %in% c(token_type$SLASH, token_type$STAR)) {
    operator <- tokens[[1]]
    right <- parse_unary(tokens[-1])

    expr <- expr_binary(expr, operator, right$expr)
    tokens <- right$tokens
  }

  list(expr = expr, tokens = tokens)
}

parse_unary <- function(tokens) {
  #cat("parse_unary\n")

  # check for unary operators
  if (tokens[[1]]$type %in% c(token_type$BANG, token_type$MINUS)) {
    operator <- tokens[[1]]
    right <- parse_unary(tokens[-1])

    expr <- expr_unary(operator, right$expr)
    tokens <- right$tokens
    return(list(expr = expr, tokens = tokens))
  }

  # parse as primary expression
  parse_primary(tokens)
}

parse_primary <- function(tokens) {
  #cat("parse_primary\n")
  # get next token
  type <- tokens[[1]]$type

  # check for literal values
  if (type == token_type$`FALSE`) {
    expr <- expr_literal(FALSE)
    return(list(expr = expr, tokens = tokens[-1]))
  }

  if (type == token_type$`TRUE`) {
    expr <- expr_literal(TRUE)
    return(list(expr = expr, tokens = tokens[-1]))
  }

  if (type == token_type$NIL) {
    expr <- expr_literal("nil")
    return(list(expr = expr, tokens = tokens[-1]))
  }

  if (type %in% c(token_type$NUMBER, token_type$STRING)) {
    expr <- expr_literal(tokens[[1]]$literal)
    return(list(expr = expr, tokens = tokens[-1]))
  }

  # check for parenthesis
  if (type == token_type$LEFT_PAREN) {
    r <- parse_expression(tokens[-1])
    tokens <- r$tokens
    expr <- expr_grouping(r$expr)

    # check for closing paren
    tokens <- consume(tokens, token_type$RIGHT_PAREN)

    # return
    return(list(expr = expr, tokens = tokens))
  }

  # otherwise no idea
  token <- tokens[[1]]
  lox_parser_error(sprintf("cannot parse token `%s`", token$lexeme), token$line)
}

#' Consume first token if it matches type
consume <- function(tokens, type) {
  token <- tokens[[1]]
  if (token$type != type) {
    msg <- sprintf("Expected '%s'", token_symbol[[type]])
    lox_parser_error(msg, token$line)
  }

  # return tokens
  tokens[-1]
}
