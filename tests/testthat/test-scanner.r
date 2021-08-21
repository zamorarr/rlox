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
    token(token_type$VAR, token_symbol$VAR),
    token(token_type$IDENTIFIER, "a"),
    token(token_type$EQUAL, token_symbol$EQUAL),
    token(token_type$STRING, "\"apple\"", "apple"),
    token(token_type$SEMICOLON, token_symbol$SEMICOLON),
    token(token_type$VAR, token_symbol$VAR, line = 2L),
    token(token_type$IDENTIFIER, "b", line = 2L),
    token(token_type$EQUAL, token_symbol$EQUAL, line = 2L),
    token(token_type$STRING, "\"banana\"", "banana", line = 2L),
    token(token_type$EOF, token_symbol$EOF, line = 2L)
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

test_that("scan_tokens works on `or` expression", {
  actual <- scan_tokens("a or b")
  expected <- list(
    token(token_type$IDENTIFIER, "a"),
    token(token_type$OR, token_symbol$OR),
    token(token_type$IDENTIFIER, "b"),
    token(token_type$EOF, token_symbol$EOF)
  )

  expect_identical(actual, expected)
})

test_that("scan_tokens works on `and` expression", {
  actual <- scan_tokens("a and b")
  expected <- list(
    token(token_type$IDENTIFIER, "a"),
    token(token_type$AND, token_symbol$AND),
    token(token_type$IDENTIFIER, "b"),
    token(token_type$EOF, token_symbol$EOF)
  )

  expect_identical(actual, expected)
})

test_that("scan_tokens works on while statement", {
  actual <- scan_tokens("while (x) y;")
  expected <- list(
    token(token_type$WHILE, token_symbol$WHILE),
    token(token_type$LEFT_PAREN, token_symbol$LEFT_PAREN),
    token(token_type$IDENTIFIER, "x"),
    token(token_type$RIGHT_PAREN, token_symbol$RIGHT_PAREN),
    token(token_type$IDENTIFIER, "y"),
    token(token_type$SEMICOLON, token_symbol$SEMICOLON),
    token(token_type$EOF, token_symbol$EOF)
  )

  expect_identical(actual, expected)
})

# scan_tokens works on function call
test_that("scan_tokens works on a function call", {
  actual <- scan_tokens("f(x,y)")
  expected <- list(
    token(token_type$IDENTIFIER, "f"),
    token(token_type$LEFT_PAREN, token_symbol$LEFT_PAREN),
    token(token_type$IDENTIFIER, "x"),
    token(token_type$COMMA, token_symbol$COMMA),
    token(token_type$IDENTIFIER, "y"),
    token(token_type$RIGHT_PAREN, token_symbol$RIGHT_PAREN),
    token(token_type$EOF, token_symbol$EOF)
  )

  expect_identical(actual, expected)
})

test_that("scan_tokens works a function declaration", {
  actual <- scan_tokens("fun f(x,y) {x + y;}")
  expected <- list(
    token(token_type$FUN, "fun"),
    token(token_type$IDENTIFIER, "f"),
    token(token_type$LEFT_PAREN, token_symbol$LEFT_PAREN),
    token(token_type$IDENTIFIER, "x"),
    token(token_type$COMMA, token_symbol$COMMA),
    token(token_type$IDENTIFIER, "y"),
    token(token_type$RIGHT_PAREN, token_symbol$RIGHT_PAREN),
    token(token_type$LEFT_BRACE, token_symbol$LEFT_BRACE),
    token(token_type$IDENTIFIER, "x"),
    token(token_type$PLUS, token_symbol$PLUS),
    token(token_type$IDENTIFIER, "y"),
    token(token_type$SEMICOLON, token_symbol$SEMICOLON),
    token(token_type$RIGHT_BRACE, token_symbol$RIGHT_BRACE),
    token(token_type$EOF, token_symbol$EOF)
  )

  expect_identical(actual, expected)
})
