token <- function(type, lexeme, literal, line) {
  stopifnot(type %in% names(token_type))
  x <- list(type = type, lexeme =  lexeme, literal = literal, line = line)
  structure(x, class = c("token", class(x)))
}

#' @export
format.token <- function(x, ...) {
  sprintf("<token> %s %s %s", x$type, x$lexeme, x$literal)
}

#' @export
print.token <- function(x, ...) {
  cat(format.token(x), "\n")
}
