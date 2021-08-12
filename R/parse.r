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

  # return output
  statements
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
