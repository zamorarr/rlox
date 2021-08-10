expr <- function(x, subclass) {
  structure(x, class = c(subclass, "lox_expr", class(x)))
}

#' Expressions
#'
#' @param left,right tokens
#' @param operator operator token
expr_binary <- function(left, operator, right) {
  x <- list(left = left, operator = operator, right = right)
  expr(x, "lox_expr_binary")
}

#' @rdname expr_binary
expr_grouping <- function(expression) {
  x <- list(expression = expression)
  expr(x, "lox_expr_grouping")
}

#' @rdname expr_binary
expr_literal <- function(value) {
  x <- list(value = value)
  expr(x, "lox_expr_literal")
}

#' @rdname expr_binary
expr_unary <- function(operator, right) {
  x <- list(operator = operator, right = right)
  expr(x, "lox_expr_unary")
}

#' @rdname expr_binary
expr_variable <- function(name) {
  x <- list(name = name)
  expr(x, "lox_expr_variable")
}

#' @export
print.lox_expr <- function(x, ...) {
  cat(format(x, ...), "\n")
}


#' @export
format.lox_expr_grouping <- function(x, pad = 0, ...) {
  space <- paste(rep(" ", pad), collapse = "")
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("|-%s`(`\n|--%s%s",
          dash,
          dash,
          format(x$expression, pad = pad + 1))
}

#' @export
format.lox_expr_binary <- function(x, pad = 0, ...) {
  space <- paste(rep(" ", pad), collapse = "")
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("|-%s`%s`\n|--%s%s\n|--%s%s",
          dash, x$operator$lexeme,
          dash,format(x$left, pad = pad + 1),
          dash,format(x$right, pad = pad + 1))
}

#' @export
format.lox_expr_unary <- function(x, pad = 0, ...) {
  space <- paste(rep(" ", pad), collapse = "")
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("|-%s`%s`\n|--%s%s",
          dash, x$operator$lexeme,
          dash, format(x$right, pad = pad + 1))
}

#' @export
format.lox_expr_literal <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("|-%s %s", dash, x$value)
}

#' @export
format.lox_expr_variable <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("|-%s %s", dash, x$name$lexeme)
}
