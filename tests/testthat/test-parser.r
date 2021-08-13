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

test_that("parse_tokens works for or expression", {
  tokens <- scan_tokens("a or b;")
  actual <- parse_tokens(tokens)
  expected <- list(stmt_expression(expr_logical(
    left = expr_variable(token(token_type$IDENTIFIER, "a")),
    operator = token(token_type$OR, token_symbol$OR),
    right = expr_variable(token(token_type$IDENTIFIER, "b"))
  )))

  expect_identical(actual, expected)
})

test_that("parse_tokens works for and expression", {
  tokens <- scan_tokens("a and b;")
  actual <- parse_tokens(tokens)
  expected <- list(stmt_expression(expr_logical(
    left = expr_variable(token(token_type$IDENTIFIER, "a")),
    operator = token(token_type$AND, token_symbol$AND),
    right = expr_variable(token(token_type$IDENTIFIER, "b"))
  )))

  expect_identical(actual, expected)
})

test_that("parse_tokens works for while statement", {
  tokens <- scan_tokens("while (x) y;")
  actual <- parse_tokens(tokens)
  expected <- list(stmt_while(
    condition = expr_variable(token(token_type$IDENTIFIER, "x")),
    body = stmt_expression(expr_variable(token(token_type$IDENTIFIER, "y")))
  ))

  expect_identical(actual, expected)
})

test_that("parse_tokens converts for loop to while loop", {
  tokens <- scan_tokens("for (var i = 0; i < 10; i = i + 1) x;")
  actual <- parse_tokens(tokens)
  expected <- list(stmt_block(list(
    # var i = 0
    stmt_variable(name = token(token_type$IDENTIFIER, "i"), initializer = expr_literal(0)),
    stmt_while(
      # i < 10
      condition = expr_binary(
        left = expr_variable(token(token_type$IDENTIFIER, "i")),
        operator = token(token_type$LESS, token_symbol$LESS),
        right = expr_literal(10)
      ),
      body = stmt_block(list(
        # x
        stmt_expression(expr_variable(token(token_type$IDENTIFIER, "x"))),
        # i = i + 1
        stmt_expression(expr_assignment(
          name = token(token_type$IDENTIFIER, "i"),
          value = expr_binary(
            left = expr_variable(token(token_type$IDENTIFIER, "i")),
            operator = token(token_type$PLUS, token_symbol$PLUS),
            right = expr_literal(1)
          )
        ))
      ))
    )
  )))

  expect_identical(actual, expected)
})
