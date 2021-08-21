#' Expressions
expr <- function(x, subclass) {
  structure(x, class = c(subclass, "lox_expr", class(x)))
}

#' @param name token
#' @param value expression
#' @rdname expr
expr_assignment <- function(name, value) {
  x <- list(name = name, value = value)
  expr(x, "lox_expr_assignment")
}

#' @param left,right expressions
#' @param operator operator token
#' @rdname expr
expr_binary <- function(left, operator, right) {
  x <- list(left = left, operator = operator, right = right)
  expr(x, "lox_expr_binary")
}

#' @param callee expression
#' @param paren token
#' @param arguments list of expressions
#' @rdname expr
expr_call <- function(callee, paren, arguments) {
  x <- list(callee = callee, paren = paren, arguments = arguments)
  expr(x, "lox_expr_call")
}

#' @rdname expr
expr_grouping <- function(expression) {
  x <- list(expression = expression)
  expr(x, "lox_expr_grouping")
}

#' @rdname expr
expr_literal <- function(value) {
  x <- list(value = value)
  expr(x, "lox_expr_literal")
}

#' @rdname expr
#' @param left,right expression
#' @param operator token
expr_logical <- function(left, operator, right) {
  x <- list(left = left, operator = operator, right = right)
  expr(x, "lox_expr_logical")
}

#' @rdname expr
expr_unary <- function(operator, right) {
  x <- list(operator = operator, right = right)
  expr(x, "lox_expr_unary")
}

#' @param name token
#' @rdname expr
expr_variable <- function(name) {
  x <- list(name = name)
  expr(x, "lox_expr_variable")
}

#' @export
print.lox_expr <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.lox_expr_assignment <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s||-`<-`\n%s|-- %s\n%s",
          dash,
          dash, x$name$lexeme,
          format(x$value, pad = pad))
}

#' @export
format.lox_expr_binary <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s||-`%s`\n%s\n%s",
          dash, x$operator$lexeme,
          format(x$left, pad = pad),
          format(x$right, pad = pad))
}

#' @export
format.lox_expr_call <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("%s||-%s", dash, format(x$callee, pad = pad))

  for (arg in x$arguments) {
    s2 <- sprintf("%s|--%s", dash, format(arg, pad = pad))
    s <- paste(s, s2, sep = "\n")
  }

  s
}


#' @export
format.lox_expr_grouping <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s||-`(`\n%s",
          dash,
          format(x$expression, pad = pad + 1))
}

#' @export
format.lox_expr_literal <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  val <- x$value
  if (is.character(val)) val <- sprintf("\"%s\"", val)
  sprintf("%s|-- %s", dash, val)
}

#' @export
format.lox_expr_logical <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s||-`%s`\n%s\n%s",
          dash, x$operator$lexeme,
          format(x$left, pad = pad),
          format(x$right, pad = pad))
}

#' @export
format.lox_expr_unary <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s||-`%s`\n%s",
          dash, x$operator$lexeme,
          format(x$right, pad = pad))
}

#' @export
format.lox_expr_variable <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s|-- %s", dash, x$name$lexeme)
}
