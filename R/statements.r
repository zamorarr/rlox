#' Statements
#'
stmt_expression <- function(expression) {
  x <- list(expression = expression)
  structure(x, class = c("lox_stmt_expression", "lox_stmt", class(x)))
}

stmt_print <- function(expression) {
  x <- list(expression = expression)
  structure(x, class = c("lox_stmt_print", "lox_stmt", class(x)))
}

stmt_variable <- function(name, initializer) {
  x <- list(name = name, initializer = initializer)
  structure(x, class = c("lox_stmt_variable", "lox_stmt", class(x)))
}
