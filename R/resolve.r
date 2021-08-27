resolve_statements <- function(statements) {
  # create resolver object
  #r <- list(tree = tree, scopes = Stack$new(), locals = HashMap$new())
  r <- list(scopes = Stack$new(), locals = HashMap$new())
  r <- structure(r, class = c("lox_resolver", class(r)))

  resolve_list(statements, r)
  r$locals
}

is_node <- function(x) is_stmt(x) || is_expr(x)
is_list_of_nodes <- function(x) {
  if (!is_node(x) && is.list(x) && length(x) > 0) {
    is_node(x[[1]])
  } else {
    FALSE
  }
}

#' Resolve tree node
#' @param x expression or statement
#' @param r resolver object
resolve <- function(x, r) UseMethod("resolve")


resolve_list <- function(x, r) {
  for (node in x) {
    resolve(node, r)
  }
  invisible(r)
}

resolve_local <- function(expr, name, r) {
  scopes <- r$scopes
  for (i in rev(seq_len(scopes$size))) {
    if (scopes$get(i)$has(name$lexeme)) {
      id <- attr(expr, "id")
      r$locals$put(id, scopes$size - i + 1L)
      break
    }
  }

  invisible(r)
}

resolve_function <- function(fun, r) {
  # open scope
  begin_scope(r)

  # resolve parameters
  for (param in fun$params) {
    declare(param, r)
    define(param, r)
  }

  # resolve body
  resolve_list(fun$body, r)

  # close scope
  end_scope(r)
  invisible(r)
}

begin_scope <- function(resolver) {
  resolver$scopes$push(HashMap$new())
  invisible(resolver)
}

end_scope <- function(resolver) {
  resolver$scopes$pop()
  invisible(resolver)
}

# declare token in scope
declare <- function(name, r) {
  if (r$scopes$is_empty()) return(invisible(r))

  scope <- r$scopes$peek()
  scope$put(name$lexeme, FALSE)
  invisible(r)
}

# define token in scope
define <- function(name, r) {
  if (r$scopes$is_empty()) return(invisible(r))

  scope <- r$scopes$peek()
  scope$put(name$lexeme, TRUE)
  invisible(r)
}
