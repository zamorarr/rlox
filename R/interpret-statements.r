#' @export
execute <- function(x, env) UseMethod("execute")

#' @export
execute_block <- function(statements, env) {
  # initialize a new environment for this block
  # the parent is the enclosing environment
  block_env <- env_new(env)
  for (statement in statements) {
    execute(statement, block_env)
  }

  invisible(env)
}

#' @export
execute.lox_stmt_print <- function(x, env) {
  value <- evaluate(x$expression, env)
  if (is.logical(value)) value <- tolower(format(value))
  cat(format(value), "\n", sep = "")
}

#' @export
execute.lox_stmt_expression <- function(x, env) {
  evaluate(x$expression, env)
}

#' @export
execute.lox_stmt_function <- function(x, env) {
  # convert declaration to function
  lf <- lox_function(x, env)

  # add function to environment
  env_define(env, x$name$lexeme, lf)

  # return something
  invisible(env)
}

#' @export
execute.lox_stmt_variable <- function(x, env) {
  value <- NULL

  # evaluate intializer
  if (!is.null(x$initializer)) {
    value <- evaluate(x$initializer, env)
  }

  # assign to environment
  env_define(env, x$name$lexeme, value)
  invisible(env)
}

#' @export
execute.lox_stmt_block <- function(x, env) {
  execute_block(x$statements, env)
}

#' @export
execute.lox_stmt_if <- function(x, env) {
  if (is_truthy(evaluate(x$condition, env))) {
    execute(x$then_branch, env)
  } else if (!is.null(x$else_branch)) {
    execute(x$else_branch, env)
  }

  invisible(env)
}

#' @export
execute.lox_stmt_return <- function(x, env) {
  value <- NULL
  if (!is.null(x$value)) value <- evaluate(x$value, env)
  lox_return(value)
}

#' @export
execute.lox_stmt_while <- function(x, env) {
  while(is_truthy(evaluate(x$condition, env))) {
    execute(x$body, env)
  }
}
