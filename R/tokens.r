token <- function(type, lexeme, literal, line) {
  stopifnot(type %in% names(token_type))
  x <- list(type = type, lexeme =  lexeme, literal = literal, line = line)
  structure(x, class = c("token", class(x)))
}

is_type <- function(token, type) {
  identical(token$type, type)
}

#' @export
format.token <- function(x, ...) {
  sprintf("<token:%i> %s %s %s", x$line, x$type, x$lexeme, x$literal)
}

#' @export
print.token <- function(x, ...) {
  cat(format.token(x), "\n")
}
