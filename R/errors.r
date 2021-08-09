lox_error2 <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(message = message, call = call, ...),
    class = c(.subclass, "error", "condition")
  )

  stop(err)
}

lox_scanner_error <- function(message, line) {
  message <-  sprintf("[scanner error] at line %i: %s", line, message)
  lox_error2("scanner_error", message = message)
}

lox_parser_error <- function(message, line) {
  message <-  sprintf("[parser error] at line %i: %s", line, message)
  lox_error2("parser_error", message = message)
}

lox_runtime_error <- function(message, token) {
  message <-  sprintf("[runtime error] at line %i: %s", token$line, message)
  lox_error2("runtime_error", message = message)
}
