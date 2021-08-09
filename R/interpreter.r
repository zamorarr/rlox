interpret <- function(x) {
  value <- evaluate(x)
  if (is.logical(value)) value <- tolower(as.character(value))
  cat(as.character(value), "\n")
}

#' @export
evaluate <- function(x) UseMethod("evaluate")

#' @export
evaluate.lox_expr_literal <- function(x) {
  x$value
}

#' @export
evaluate.lox_expr_grouping <- function(x) {
  evaluate(x$expression)
}

#' @export
evaluate.lox_expr_unary <- function(x) {
  right <- evaluate(x$right)

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
evaluate.lox_expr_binary <- function(x) {
  left <- evaluate(x$left)
  right <- evaluate(x$right)

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

is_truthy <- function(x) {
  if (x == "nil") FALSE
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
