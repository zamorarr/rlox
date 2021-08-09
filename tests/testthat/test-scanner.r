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
