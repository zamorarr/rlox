#' Create Lox  Function
#' @param declaration stmt_function
#' @param closure env
lox_function <- function(declaration, closure) {
  # create callable
  x <- callable(
    arity = length(declaration$params),
    f = 0,
    formatter = function() sprintf("<fn %s>", declaration$name$lexeme))

  # add declaration and closure
  x$declaration <- declaration
  x$closure <- closure

  # create lox_function object
  structure(x, class = c("lox_function", class(x)))
}

lox_call.lox_function <- function(fn, arguments, env) {
  # we actually ignore the calling environment and instead use the closure
  env <- env_new(fn$closure)

  # get declaration
  declaration <- fn$declaration

  # bind arguments to params in env
  for (i in seq_along(declaration$params)) {
    env_define(env, name = declaration$params[[i]]$lexeme, value = arguments[[i]])
  }

  # should we be evaluating the body as a statement block?
  #evaluate(stmt_block(declaration$body), env)
  res <- tryCatch(
    # if it finds a return statement, stops immediately and comes here
    "return" = function(cnd) cnd$value,

    # evaluate each statement and return NULL if no return statement
    {
      for (statement in declaration$body) {
        evaluate(statement, env)
      }
      return(NULL)
    }
  )

  # return result
  res
}
