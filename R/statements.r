#' Statements
stmt <- function(x, subclass) {
  structure(x, class = c(subclass, "lox_stmt", class(x)))
}

#' @param statements list of statements
stmt_block <- function(statements) {
  x <- list(statements = statements)
  stmt(x, "lox_stmt_block")
}


#' @param expression expression
stmt_expression <- function(expression) {
  x <- list(expression = expression)
  stmt(x, "lox_stmt_expression")
}

#' Print Statement
#' @param expression expression
stmt_print <- function(expression) {
  x <- list(expression = expression)
  stmt(x, "lox_stmt_print")
}


#' Variable Statement
#' @param name token
#' @param initializer expression
stmt_variable <- function(name, initializer = NULL) {
  x <- list(name = name, initializer = initializer)
  stmt(x, "lox_stmt_variable")
}

#' @export
print.lox_stmt <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.lox_stmt_expression <- function(x, pad = 0, ...) {
  format(x$expression, pad = pad)
}

format.lox_stmt_print <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("|-%s`%s`\n|--%s%s",
          dash, "print",
          dash, format(x$expression, pad = pad + 1))
}

#' @export
format.lox_stmt_variable <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("|-%s`var` %s", dash, x$name$lexeme)

  # show initialization if there is one
  if (!is.null(x$initializer)) {
    s2 <- sprintf("\n|--%s%s", dash, format(x$initializer, pad = pad + 1))
    s <- paste0(s, s2)
  }

  s
}

#' @export
format.lox_stmt_block <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("|-%s`{`", dash)
  for (stmt in x$statements) {
    s <- paste0(s, sprintf("\n|--%s%s", dash, format(stmt)))
  }
  s
}
