#' @export
evaluate.lox_expr_literal <- function(x, env = NULL) {
  x$value
}

#' @export
evaluate.lox_expr_grouping <- function(x, env = NULL) {
  evaluate(x$expression, env)
}

#' @export
evaluate.lox_expr_unary <- function(x, env = NULL) {
  right <- evaluate(x$right, env)

  type <- x$operator$type
  if (type == token_type$BANG) {
    !is_truthy(right)
  } else if (type == token_type$MINUS) {
    check_number_operand(x$operator, right)
    -as.double(right)
  } else {
    lox_runtime_error(
      sprintf("Cannot evaluate unary expression \n%s", format(x)),
      x$operator
    )
  }
}

#' @export
evaluate.lox_expr_binary <- function(x, env = NULL) {
  left <- evaluate(x$left, env)
  right <- evaluate(x$right, env)

  type <- x$operator$type

  if (type == token_type$BANG_EQUAL) {
    !is_equal(left, right)
  } else if (type == token_type$EQUAL_EQUAL) {
    is_equal(left, right)
  } else if (type == token_type$GREATER) {
    check_number_operands(x$operator, left, right)
    as.double(left) > as.double(right)
  } else if (type == token_type$GREATER_EQUAL) {
    check_number_operands(x$operator, left, right)
    as.double(left) >= as.double(right)
  } else if (type == token_type$LESS) {
    check_number_operands(x$operator, left, right)
    as.double(left) < as.double(right)
  } else if (type == token_type$LESS_EQUAL) {
    check_number_operands(x$operator, left, right)
    as.double(left) <= as.double(right)
  } else if (type == token_type$MINUS) {
    check_number_operands(x$operator, left, right)
    as.double(left) - as.double(right)
  } else if (type == token_type$SLASH) {
    check_number_operands(x$operator, left, right)
    as.double(left) / as.double(right)
  } else if (type == token_type$STAR) {
    check_number_operands(x$operator, left, right)
    as.double(left) * as.double(right)
  } else if (type == token_type$PLUS){
    if (is.numeric(left) && is.numeric(right)) {
      as.double(left) + as.double(right)
    } else if (is.character(left) && is.character(right)) {
      paste0(left, right, collapse = " ")
    } else {
      lox_runtime_error(
        sprintf("Operands must be two numbers or two strings: %s %s %s",
                left, x$operator$lexeme, right),
        x$operator
      )
    }
  } else {
    lox_runtime_error(
      sprintf("Cannot evaluate binary expression \n%s", format(x)),
      x$operator
    )
  }
}

#' @export
evaluate.lox_expr_variable <- function(x, env) {
  env_get(env, x$name)
}

#' @export
evaluate.lox_expr_assignment <- function(x, env) {
  # evaluate expression
  val <- evaluate(x$value)

  # assign value to name
  env_assign(env, x$name, val)

  # return value
  val
}
