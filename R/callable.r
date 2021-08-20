callable <- function(arity, f, formatter = NULL) {
  if (is.null(formatter)) {
    formatter <- function(f) paste(format(f), collapse = "\n")
  }

  x <- list(arity = arity, f = f, formatter = formatter)
  structure(x, class = c("lox_callable", class(x)))
}

lox_call <- function(f, arguments, env) UseMethod("lox_call")
lox_call.lox_callable <- function(callee, arguments, env) {
  do.call(callee$f, arguments, envir = env)
}

arity <- function(callee) UseMethod("arity")
arity.lox_callable <- function(callee) {
  callee$arity
}

format.lox_callable <- function(x) {
  x$formatter(x$f)
}
