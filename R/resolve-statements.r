resolve.lox_stmt_block <- function(x, r) {
  begin_scope(r)
  for (statement in x$statements) {
    resolve(statement, r)
  }
  end_scope(r)
  invisible(r)
}

resolve.lox_stmt_expression <- function(x, r) {
  resolve(x$expression, r)
  invisible(r)
}

resolve.lox_stmt_function <- function(x, r) {
  declare(x$name, r)
  define(x$name, r)

  resolve_function(x, function_type$FUNCTION, r)
  invisible(r)
}

resolve.lox_stmt_if <- function(x, r) {
  resolve(x$condition, r)
  resolve(x$then_branch, r)
  if (!is.null(x$else_branch)) resolve(x$else_branch, r)
  invisible(r)
}

resolve.lox_stmt_print <- function(x, r) {
  resolve(x$expression, r)
  invisible(r)
}

resolve.lox_stmt_return <- function(x, r) {
  # check if inside a function
  if (identical(r$fun_type, function_type$NONE)) {
    lox_resolver_error("Cannot return from top-level code", x$keyword$line)
  }

  # check if has value
  if (!is.null(x$value)) resolve(x$value, r)
  invisible(r)
}

resolve.lox_stmt_variable <- function(x, r) {
  declare(x$name, r)
  if (!is.null(x$initializer)) {
    resolve(x$initializer, r)
  }
  define(x$name, r)
  invisible(r)
}

resolve.lox_stmt_while <- function(x, r) {
  resolve(x$condition, r)
  resolve(x$body, r)
  invisible(r)
}
