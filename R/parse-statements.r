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

  # for statement
  if (is_type(token, token_type$FOR)) {
    return(parse_statement_for(tokens[-1]))
  }

  # if statement
  if (is_type(token, token_type$IF)) {
    return(parse_statement_if(tokens[-1]))
  }

  # print statement
  if (is_type(token, token_type$PRINT)) {
    return(parse_statement_print(tokens[-1]))
  }

  # while statement
  if (is_type(token, token_type$WHILE)) {
    return(parse_statement_while(tokens[-1]))
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

parse_statement_for <- function(tokens) {
  # check for closing )
  tokens <- consume(tokens, token_type$LEFT_PAREN)

  # get next token
  token <- tokens[[1]]

  # determine initializer
  initializer <- NULL
  if (is_type(token, token_type$SEMICOLON)) {
    # no initializer
    initializer <- NULL
    tokens <- tokens[-1]
  } else if (is_type(token, token_type$VAR)) {
    # var declaration (ex. var i = 0)
    p <- parse_vardeclaration(tokens[-1])
    initializer <- p$stmt
    tokens <- p$tokens
  } else {
    # statement expression (ex. ;)
    initializer <- parse_statement_expression(tokens[-1])
    initializer <- p$stmt
    tokens <- p$tokens
  }

  # determine loop condition
  token <- tokens[[1]]
  condition <- NULL
  if (!is_type(token, token_type$SEMICOLON)) {
    p <- parse_expression(tokens)
    condition <- p$expr
    tokens <- p$tokens
  }

  # check for semicolon
  tokens <- consume(tokens, token_type$SEMICOLON)

  # determine increment
  increment <- NULL
  if (!is_type(token, token_type$RIGHT_PAREN)) {
    p <- parse_expression(tokens)
    increment <- p$expr
    tokens <- p$tokens
  }

  # check for closing )
  tokens <- consume(tokens, token_type$RIGHT_PAREN)

  # determine body
  p <- parse_statement(tokens)
  body <- p$stmt
  tokens <- p$tokens

  # we've parsed all parts of the for loop
  # now just de-sugar it by converting to a while loop

  # add the increment to bottom of the while loop
  if (!is.null(increment)) {
    body <- stmt_block(list(
      body,
      stmt_expression(increment)
    ))
  }

  # add condition to while loop
  if (is.null(condition)) condition <- expr_literal(TRUE)
  body <- stmt_while(condition, body)

  # add initializer to top of body
  if (!is.null(initializer)) {
    body <- stmt_block(list(initializer, body))
  }

  # return
  list(stmt = body, tokens = tokens)
}

parse_statement_while <- function(tokens) {
  # check for opening (
  tokens <- consume(tokens, token_type$LEFT_PAREN)

  # parse condition
  p <- parse_expression(tokens)
  condition <- p$expr
  tokens <- p$tokens

  # check for closing )
  tokens <- consume(tokens, token_type$RIGHT_PAREN)

  # parse body
  p <- parse_statement(tokens)
  body <- p$stmt
  tokens <- p$tokens

  list(stmt = stmt_while(condition, body), tokens = tokens)
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
