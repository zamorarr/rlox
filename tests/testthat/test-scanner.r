test_that("scan_tokens works on valid expression", {
  actual <- scan_tokens("3 + 4")
  expected <- list(
    token(token_type$NUMBER, "3", 3, line = 1L),
    token(token_type$PLUS, "+", "", line = 1L),
    token(token_type$NUMBER, "4", 4, line = 1L),
    token(token_type$EOF, "", "", line = 1L)
  )

  expect_identical(actual, expected)
})

test_that("scan_tokens stops when invalid token found", {
  expect_error(scan_tokens("#"), )
})

test_that("scan token works", {
  actual <- scan_tokens('var a = "apple";\nvar b = "banana"')
  expected <- list(
    token(token_type$VAR, token_symbol$VAR, "", line = 1L),
    token(token_type$IDENTIFIER, "a", "", line = 1L),
    token(token_type$EQUAL, token_symbol$EQUAL, "", line = 1L),
    token(token_type$STRING, "\"apple\"", "apple", line = 1L),
    token(token_type$SEMICOLON, token_symbol$SEMICOLON, "", line = 1L),
    token(token_type$VAR, token_symbol$VAR, "", line = 2L),
    token(token_type$IDENTIFIER, "b", "", line = 2L),
    token(token_type$EQUAL, token_symbol$EQUAL, "", line = 2L),
    token(token_type$STRING, "\"banana\"", "banana", line = 2L),
    token(token_type$EOF, token_symbol$EOF, "", line = 2L)
  )

  expect_identical(actual, expected)
})

test_that("scan_tokens works on if statement", {
  actual <- scan_tokens("if (condition) x else y")
  expected <- list(
    token(token_type$IF, token_symbol$IF),
    token(token_type$LEFT_PAREN, token_symbol$LEFT_PAREN),
    token(token_type$IDENTIFIER, "condition"),
    token(token_type$RIGHT_PAREN, token_symbol$RIGHT_PAREN),
    token(token_type$IDENTIFIER, "x"),
    token(token_type$ELSE, "else"),
    token(token_type$IDENTIFIER, "y"),
    token(token_type$EOF, token_symbol$EOF)
  )

  expect_identical(actual, expected)
})
