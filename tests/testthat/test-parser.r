test_that("parse_expression throws error for bad syntax", {
  tokens <- scan_tokens("4!")
  expect_error(parse_tokens(tokens))

  #p <- parse_tokens(scan_tokens('var a = "global a";\nvar b = "global b"'))
})
