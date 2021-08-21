parse_declaration <- function(tokens) {
  token <- tokens[[1]]

  if (token$type == token_type$FUN) {
    return(parse_fundeclaration(tokens[-1], "function"))
  }

  if (token$type == token_type$VAR) {
    return(parse_vardeclaration(tokens[-1]))
  }

  # parse statement
  parse_statement(tokens)
}

parse_fundeclaration <- function(tokens, kind) {
  # get name
  name <- tokens[[1]]
  tokens <- consume(tokens, token_type$IDENTIFIER)

  # consume left paren
  tokens <- consume(tokens, token_type$LEFT_PAREN)

  # get params
  parameters <- list()
  if (!is_type(tokens[[1]], token_type$RIGHT_PAREN)) {
    while(TRUE) {
      if (length(parameters) >= 255) {
        lox_parser_error("Cannot have more than 255 parameters", tokens[[1]]$line)
      }

      # add parameter
      token <- tokens[[1]]
      tokens <- consume(tokens, token_type$IDENTIFIER)
      parameters <- c(parameters, list(token))

      # check if done
      if (!is_type(tokens[[1]], token_type$COMMA)) break
      tokens <- consume(tokens, token_type$COMMA)
    }
  }

  # consume right paren
  tokens <- consume(tokens, token_type$RIGHT_PAREN)

  # consume left brace
  tokens <- consume(tokens, token_type$LEFT_BRACE)

  # get body
  p <- parse_statement_block(tokens)
  body <- p$stmt
  tokens <- p$tokens

  # body <- stmt_block(body) # we don't want body wrapped in stmt_block? why not?
  # this is why: https://github.com/munificent/craftinginterpreters/blob/master/note/answers/chapter10_functions.md
  # Lox uses the same scope for the parameters and local variables immediately inside the body.
  # That's why Stmt.Function stores the body as a list of statements, not a single Stmt.
  # Block that would create its own nested scope separate from the parameters.

  # return
  list(stmt = stmt_function(name, parameters, body), tokens = tokens)
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

  # return statement
  if (is_type(token, token_type$RETURN)) {
    return(parse_statement_return(tokens))
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

parse_statement_return <- function(tokens) {
  # get return token
  keyword <- tokens[[1]]
  tokens <- tokens[-1]

  # parse value
  value <- NULL
  if (!is_type(tokens[[1]], token_type$SEMICOLON)) {
    p <- parse_expression(tokens)
    value <- p$expr
    tokens <- p$tokens
  }

  # check for semicolon
  tokens <- consume(tokens, token_type$SEMICOLON)
  list(stmt = stmt_return(keyword, value), tokens = tokens)
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
