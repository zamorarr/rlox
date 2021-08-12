parse_declaration <- function(tokens) {
  token <- tokens[[1]]
  if (token$type == token_type$VAR) {
    return(parse_vardeclaration(tokens[-1]))
  }

  # parse statement
  parse_statement(tokens)
}

parse_vardeclaration <- function(tokens) {
  name <- tokens[[1]]
  tokens <- consume(tokens, token_type$IDENTIFIER)

  initializer <- NULL
  if (tokens[[1]]$type == token_type$EQUAL) {
    p <- parse_expression(tokens[-1])
    initializer <- p$expr
    tokens <- p$tokens
  }

  tokens <- consume(tokens, token_type$SEMICOLON)
  list(stmt = stmt_variable(name, initializer), tokens = tokens)
}

parse_statement <- function(tokens) {
  # get first token
  token <- tokens[[1]]

  # if statement
  if (is_type(token, token_type$IF)) {
    return(parse_statement_if(tokens[-1]))
  }

  # print statement
  if (is_type(token, token_type$PRINT)) {
    return(parse_statement_print(tokens[-1]))
  }

  # block statement
  if (is_type(token, token_type$LEFT_BRACE)) {
    # statement block returns a raw list of statements.
    # need to wrap those in block
    p <- parse_statement_block(tokens[-1])
    return(list(stmt = stmt_block(p$stmts), tokens = p$tokens))
  }

  # expression statement
  parse_statement_expression(tokens)
}

parse_statement_if <- function(tokens) {
  # look for opening (
  tokens <- consume(tokens, token_type$LEFT_PAREN)

  # parse condition
  p <- parse_expression(tokens)
  condition <- p$expr
  tokens <- p$tokens

  # look for closing )
  tokens <- consume(tokens, token_type$RIGHT_PAREN)

  # parse then_branch
  p <- parse_statement(tokens)
  then_branch <- p$stmt
  tokens <- p$tokens

  # look for else branch
  else_branch <- NULL
  if (is_type(tokens[[1]], token_type$ELSE)) {
    p <- parse_statement(tokens[-1])
    else_branch <- p$stmt
    tokens <- p$tokens
  }

  # create if statement
  list(stmt = stmt_if(condition, then_branch, else_branch), tokens = tokens)
}

parse_statement_print <- function(tokens) {
  p <- parse_expression(tokens)
  expr <- p$expr
  tokens <- p$tokens

  tokens <- consume(tokens, token_type$SEMICOLON)
  list(stmt = stmt_print(expr), tokens = tokens)
}

parse_statement_block <- function(tokens) {
  statements <- list()

  i <- 1L
  while(!is_type(tokens[[1]], token_type$RIGHT_BRACE) && length(tokens) > 0) {
    p <- parse_declaration(tokens)
    stmt <- p$stmt
    tokens <- p$tokens

    statements[[i]] <- stmt
    i <- i + 1L
  }

  # check for closing }
  tokens <- consume(tokens, token_type$RIGHT_BRACE)
  list(stmts = statements, tokens = tokens)
}

parse_statement_expression <- function(tokens) {
  p <- parse_expression(tokens)
  expr <- p$expr
  tokens <- p$tokens

  tokens <- consume(tokens, token_type$SEMICOLON)
  list(stmt = stmt_expression(expr), tokens = tokens)
}
