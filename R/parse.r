parse_tokens <- function(tokens) {
  # initialize output
  statements <- list()

  i <- 1L
  while(length(tokens) > 1) {
    p <- parse_declaration(tokens)
    statements[[i]] <- p$stmt
    tokens <- p$tokens
    i <- i + 1L
  }

  # add id to each node
  statements <- enumerate_tree(statements)$branch

  # return output
  statements
}

#' Descent the tree and enumerate each node
#'
#' @param tree abstract syntax tree from \code{parse_tokens}
#' @param id initial id
enumerate_tree <- function(tree, id = 1L) {
  for (i in seq_along(tree)) {
    branch <- tree[[i]]

    # what about inhereting list of statements?
    if (is_node(branch) || is_list_of_nodes(branch)) {
      # add id to statement/expression
      attr(branch, "id") <- id
      # branch$id <- id

      # recurse down the tree (incrementing id)
      p <- enumerate_tree(branch, id + 1L)

      # pull up tree and id
      tree[[i]] <- p$branch
      id <- p$id
    }
  }

  list(branch = tree, id = id)
}

#' Consume first token if it matches type
consume <- function(tokens, type) {
  token <- tokens[[1]]
  if (token$type != type) {
    msg <- sprintf("Expected '%s'", token_symbol[[type]])
    lox_parser_error(msg, token$line)
  }

  # return tokens
  tokens[-1]
}
