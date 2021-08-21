interpret <- function(statements, env = NULL) {
  # create environment if it doesn't exist
  #env <- rlang::new_environment()
  if (is.null(env)) env <- env_new()

  # define built-in global functions
  # this should go somewhere else probably
  if (!env_has(env, "clock")) {
    env_define(
      env,
      "clock",
      callable(0, function() as.double(Sys.time()), function() "<native fn>")
    )
  }

  # evaluate statements in environment
  for (statement in statements) {
    evaluate(statement, env)
  }

  # debug show env
  #rlang::env_print(env)

  # return env? not necessary because env is modified in place
  #env
  invisible(env)
}

#' @export
evaluate <- function(x, env) UseMethod("evaluate")

is_truthy <- function(x) {
  if (x == "nil") return(FALSE)
  if (is.logical(x)) return(x)
  TRUE
}

is_equal <- function(x, y) {
  if (x == "nil" && y == "nil") return(TRUE)
  if (x == "nil" || y == "nil") return(FALSE)
  x == y
}

check_number_operand <- function(operator, operand) {
  if (is.double(operand)) return()
  lox_runtime_error(sprintf("Operand %s must be number", operand), operator)

}

check_number_operands <- function(operator, left, right) {
  if (is.double(left) && is.double(right)) return()
  lox_runtime_error(sprintf("Operands must be numbers"), operator)
}
