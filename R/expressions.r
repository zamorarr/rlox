#' Expressions
#'
#' @param left,right tokens
#' @param operator operator token
expr_binary <- function(left, operator, right) {
  x <- list(left = left, operator = operator, right = right)
  structure(x, class = c("lox_expr_binary", "lox_expr", class(x)))
}

#' @rdname expr_binary
expr_grouping <- function(expression) {
  x <- list(expression = expression)
  structure(x, class = c("lox_expr_grouping", "lox_expr", class(x)))
}

#' @rdname expr_binary
expr_literal <- function(value) {
  x <- list(value = value)
  structure(x, class = c("lox_expr_literal", "lox_expr", class(x)))
}

#' @rdname expr_binary
expr_unary <- function(operator, right) {
  x <- list(operator = operator, right = right)
  structure(x, class = c("lox_expr_unary", "lox_expr", class(x)))
}

#parse_expression(scan_tokens("(-1 == 10) * ((9 - 3) + 7)"))

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
print.lox_expr <- function(x, ...) {
  cat(format(x, ...), "\n")
}
