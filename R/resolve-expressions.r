
resolve.lox_expr_assign <- function(x, r) {
  resolve(x$value, r)
  resolve_local(x, x$name, r)
  invisible(r)
}

resolve.lox_expr_binary <- function(x, r) {
  resolve(x$left, r)
  resolve(x$right, r)
  invisible(r)
}

resolve.lox_expr_call <- function(x, r) {
  resolve(x$callee, r)

  for (argument in x$arguments) {
    resolve(argument, r)
  }

  invisible(r)
}

resolve.lox_expr_grouping <- function(x, r) {
  resolve(x$expression, r)
  invisible(r)
}

resolve.lox_expr_literal <- function(x, r) {
  invisible(r)
}

resolve.lox_expr_logical <- function(x, r) {
  resolve(x$left, r)
  resolve(x$right, r)
  invisible(r)
}

resolve.lox_expr_unary <- function(x, r) {
  resolve(x$right, r)
  invisible(r)
}

resolve.lox_expr_variable <- function(x, r) {
  scopes <- r$scopes
  if (!scopes$is_empty()) {
    scope <- scopes$peek()
    if (scope$has(x$name$lexeme) && !scope$get(x$name$lexeme)) {
      lox_resolver_error(sprintf("Cannot read local variable `%s` in its own initializer", x$name$lexeme), x$name$line)
    }
  }

  resolve_local(x, x$name, r)
}

resolve.lox_expr_assign <- function(x, r) {
  resolve(x$value, r)
  resolve_local(x, x$name, r)
  insvisible(r)
}

resolve.lox_expr_literal <- function(x, r) {
  invisible(r)
}
