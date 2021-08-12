#' @export
evaluate.lox_stmt_print <- function(x, env = NULL) {
  value <- evaluate(x$expression, env)
  if (is.logical(value)) value <- tolower(as.character(value))
  cat(as.character(value), "\n")
}

#' @export
evaluate.lox_stmt_expression <- function(x, env = NULL) {
  evaluate(x$expression, env)
}

#' @export
evaluate.lox_stmt_variable <- function(x, env) {
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
evaluate.lox_stmt_block <- function(x, env) {
  # initialize a new environment for this block
  # the parent is the enclosing environment
  block_env <- env_new(env)
  for (stmt in x$statements) {
    evaluate(stmt, block_env)
  }

  invisible(env)
}
