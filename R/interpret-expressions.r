#' @export
evaluate <- function(x, interpreter) UseMethod("evaluate")

#' @export
evaluate.lox_expr_literal <- function(x, interpreter) {
  x$value
}

#' @export
evaluate.lox_expr_logical <- function(x, interpreter) {
  left <- evaluate(x$left, interpreter)

  if (is_type(x$operator, token_type$OR)) {
    # short circuit OR
    if (is_truthy(left)) return(left)
  } else {
    # short circuit AND
    if (!is_truthy(left)) return(left)
  }

  evaluate(x$right, interpreter)
}

#' @export
evaluate.lox_expr_grouping <- function(x, interpreter) {
  evaluate(x$expression, interpreter)
}

#' @export
evaluate.lox_expr_unary <- function(x, interpreter) {
  right <- evaluate(x$right, interpreter)

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
evaluate.lox_expr_binary <- function(x, interpreter) {
  left <- evaluate(x$left, interpreter)
  right <- evaluate(x$right, interpreter)

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
evaluate.lox_expr_variable <- function(x, interpreter) {
  #env_get(interpreter$env_cur, x$name)
  lookup_variable(x$name, x, interpreter)
}

#' Look-up variable in locals table
#' @param x expression
#' @param name token
#' @param interpreter interpreter
lookup_variable <- function(name, x, interpreter) {
  id <- attr(x, "id")
  if (interpreter$locals$has(id)) {
    depth <- interpreter$locals$get(id)
    env_get_at(interpreter$env_cur, name, depth)
  } else {
    env_get(interpreter$env_global, name, inherits = FALSE)
  }
}

#' @export
evaluate.lox_expr_assignment <- function(x, interpreter) {
  # evaluate expression
  val <- evaluate(x$value, interpreter)

  # assign value to name
  #env_assign(env, x$name, val)
  assign_variable(x$name, val, x, interpreter)
}

#' Assign variable in locals table
#' @param x expression
#' @param name token
#' @param value value to assign
#' @param interpreter interpreter
assign_variable <- function(name, value, x, interpreter) {
  id <- attr(x, "id")
  if (interpreter$locals$has(id)) {
    depth <- interpreter$locals$get(id)
    env_assign_at(interpreter$env_cur, name, value, depth)
  } else {
    env_assign(interpreter$env_global, name, value)
  }
  value
}

#' @export
evaluate.lox_expr_call <- function(x, interpreter) {
  callee <- evaluate(x$callee, interpreter)
  arguments <- lapply(x$arguments, evaluate, interpreter = interpreter)

  if (!inherits(callee, "lox_callable")) {
    lox_runtime_error("Can only call functions and classes", x$paren)
  }

  if (length(arguments) != arity(callee)) {
    lox_runtime_error(
      sprintf("Expected %i arguments but got %i.", arity(callee), length(arguments)),
      x$paren
      )
  }


  # call function
  lox_call(callee, arguments, interpreter)
}
