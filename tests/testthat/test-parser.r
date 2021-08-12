test_that("parse_expression throws error for bad syntax", {
  tokens <- scan_tokens("4!")
  expect_error(parse_tokens(tokens))

  #p <- parse_tokens(scan_tokens('var a = "global a";\nvar b = "global b"'))
})

test_that("parse_tokens works for if statement", {
  tokens <- scan_tokens("if (true) x; else y;")
  actual <- parse_tokens(tokens)

  token_x <-token(token_type$IDENTIFIER, "x")
  token_y <- token(token_type$IDENTIFIER, "y")
  expected <- list(stmt_if(
    condition = expr_literal(TRUE),
    then_branch = stmt_expression(expr_variable(token_x)),
    else_branch = stmt_expression(expr_variable(token_y))
  ))

  expect_identical(actual, expected)
})
