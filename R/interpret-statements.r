#' @export
execute <- function(x, interpreter) UseMethod("execute")

#' @export
execute_block <- function(statements, interpreter) {
  # initialize a new environment for this block
  # the parent is the enclosing environment
  #block_env <- env_new(env)
  block_env <- env_new(interpreter$env_cur)
  interpreter$env_cur <- block_env
  for (statement in statements) {
    execute(statement, interpreter)
  }

  invisible(interpreter)
}

#' @export
execute.lox_stmt_print <- function(x, interpreter) {
  value <- evaluate(x$expression, interpreter)
  if (is.logical(value)) value <- tolower(format(value))
  cat(format(value), "\n", sep = "")
}

#' @export
execute.lox_stmt_expression <- function(x, interpreter) {
  evaluate(x$expression, interpreter)
}

#' @export
execute.lox_stmt_function <- function(x, interpreter) {
  # convert declaration to function
  lf <- lox_function(x, interpreter$env_cur)

  # add function to environment
  env_define(interpreter$env_cur, x$name$lexeme, lf)

  # return something
  invisible(interpreter)
}

#' @export
execute.lox_stmt_variable <- function(x, interpreter) {
  value <- NULL

  # evaluate intializer
  if (!is.null(x$initializer)) {
    value <- evaluate(x$initializer, interpreter)
  }

  # assign to environment
  env_define(interpreter$env_cur, x$name$lexeme, value)
  invisible(interpreter)
}

#' @export
execute.lox_stmt_block <- function(x, interpreter) {
  execute_block(x$statements, interpreter)
}

#' @export
execute.lox_stmt_if <- function(x, interpreter) {
  if (is_truthy(evaluate(x$condition, interpreter))) {
    execute(x$then_branch, interpreter)
  } else if (!is.null(x$else_branch)) {
    execute(x$else_branch, interpreter)
  }

  invisible(interpreter)
}

#' @export
execute.lox_stmt_return <- function(x, interpreter) {
  value <- NULL
  if (!is.null(x$value)) value <- evaluate(x$value, interpreter)
  lox_return(value)
}

#' @export
execute.lox_stmt_while <- function(x, interpreter) {
  while(is_truthy(evaluate(x$condition, interpreter))) {
    execute(x$body, interpreter)
  }
}
