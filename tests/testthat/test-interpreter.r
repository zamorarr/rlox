test_that("interpreter throws error for undefined variable", {
  tokens <- scan_tokens("d;")
  p <- parse_tokens(tokens)
  expect_error(interpret(p))
})

test_that("interpreter works for if statement", {
  x <- 'var a = 3; var b = 4;if (a > b) print "a bigger"; else print "a not bigger";'
  tokens <- scan_tokens(x)
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "a not bigger")
})


test_that("interpreter works for OR expression", {
  tokens <- scan_tokens('print nil or "yes";')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "yes")

  tokens <- scan_tokens('print true or false;')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "true")
})

test_that("interpreter works for AND expression", {
  tokens <- scan_tokens('print nil and "yes";')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "nil")

  tokens <- scan_tokens('print 1 < 2 and 10 > 3;')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "true")
})
