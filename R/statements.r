#' Statements
stmt <- function(x, subclass) {
  structure(x, class = c(subclass, "lox_stmt", class(x)))
}

#' @param statements list of statements
#' @rdname stmt
stmt_block <- function(statements) {
  x <- list(statements = statements)
  stmt(x, "lox_stmt_block")
}

#' @param expression expression
#' @rdname stmt
stmt_expression <- function(expression) {
  x <- list(expression = expression)
  stmt(x, "lox_stmt_expression")
}

#' If Statement
#' @param condition expression
#' @param then_branch statement
#' @param else_branch statement
#' @rdname stmt
stmt_if <- function(condition, then_branch, else_branch = NULL) {
  x <- list(condition = condition, then_branch = then_branch, else_branch = else_branch)
  stmt(x, "lox_stmt_if")
}

#' Print Statement
#' @param expression expression
#' @rdname stmt
stmt_print <- function(expression) {
  x <- list(expression = expression)
  stmt(x, "lox_stmt_print")
}

#' Variable Statement
#' @param name token
#' @param initializer expression
#' @rdname stmt
stmt_variable <- function(name, initializer = NULL) {
  x <- list(name = name, initializer = initializer)
  stmt(x, "lox_stmt_variable")
}


#' @param condition expression
#' @param body statement
#' @rdname stmt
stmt_while <- function(condition, body) {
  x <- list(condition = condition, body = body)
  stmt(x, "lox_stmt_while")
}

#' @export
print.lox_stmt <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.lox_stmt_block <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("%s||-`{`", dash)
  for (stmt in x$statements) {
    s <- paste0(s, sprintf("\n%s", format(stmt, pad = pad + 1L)))
  }
  s
}

#' @export
format.lox_stmt_expression <- function(x, pad = 0, ...) {
  format(x$expression, pad = pad)
}

#' @export
format.lox_stmt_if <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("%s||-`%s`\n%s\n%s",
               dash, token_symbol$IF,
               format(x$condition, pad = pad + 1L),
               format(x$then_branch, pad = pad + 1L)
  )

  if (!is.null(x$else_branch)) {
    s2 <- sprintf("\n%s|--`%s`\n%s",
                  dash, token_symbol$ELSE,
                  format(x$else_branch, pad = pad + 1L))
    s <- paste0(s, s2)
  }

  s
}

format.lox_stmt_print <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  sprintf("%s||-`%s`\n%s",
          dash, token_symbol$PRINT,
          format(x$expression, pad = pad))
}

#' @export
format.lox_stmt_variable <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("%s||-`%s`\n%s|-- %s",
               dash, token_symbol$VAR,
               dash, x$name$lexeme)

  # show initialization if there is one
  if (!is.null(x$initializer)) {
    s2 <- sprintf("\n%s", format(x$initializer, pad = pad))
    s <- paste0(s, s2)
  }

  s
}

#' @export
format.lox_stmt_while <- function(x, pad = 0, ...) {
  dash <- paste(rep("-", pad), collapse = "")
  s <- sprintf("%s||-`%s`\n%s\n%s",
               dash, token_symbol$WHILE,
               format(x$condition, pad = pad + 1L),
               format(x$body, pad = pad + 1L)
               )
}
