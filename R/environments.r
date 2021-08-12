env_new <- function(parent = emptyenv()) {
  e <- new.env(parent = parent)
  class(e) <- c("lox_env", class(e))
  e
}

#' @param env lox environment
#' @param token
env_get <- function(env, token) {
  # check that token is type variable
  if (!token$type %in% c(token_type$VAR, token_type$IDENTIFIER)) {
    msg <- sprintf("Cannot get variable `%s` from environment", token$lexeme)
    lox_runtime_error(msg, token$lexeme)
  }

  # get name of token
  name <- token$lexeme

  # check if name exists in environment (or any parent)
  if (!exists(name, envir = env, inherits = TRUE)) {
    msg <- sprintf("Undefined variable '%s'", name)
    lox_runtime_error(msg, token)
  }

  # get and return value for name
  get(name, envir = env, inherits = TRUE)
}

#' @param env lox environment
#' @param name string
#' @param value any value
env_define <- function(env, name, value) {
  # check that token is type variable
  #if (!inherits(expr, "lox_expr_variable")) {
  #  msg <- sprintf("Not a variable expression: %s", format(expr))
  #  stop(msg, call. = FALSE)
  #}

  assign(name, value, envir = env)
}

#' @param env lox environment
#' @param name string
env_has <- function(env, name) {
  exists(name, envir = env, inherits = TRUE)
}

env_assign <- function(env, token, value) {
  name <- token$lexeme
  if (env_contains(env, name)) {
    # assignment happens in the environment where the name is found
    # this can be a parent environment
    assign(name, value, envir = env, inherits = TRUE)
    return(env)
  }

  lox_runtime_error(sprintf("Undefined variable `%`.", name))
}
