test_that("interpreter works for if statement", {
  x <- 'var a = 3; var b = 4;if (a > b) print "a bigger"; else print "a not bigger";'
  tokens <- scan_tokens(x)
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "a not bigger")
})
