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

test_that("interpret works for WHILE statement", {
  tokens <- scan_tokens('var i = 1; while (i <= 3) {print i; i = i + 1;}')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "1\\n2\\n3")
})

test_that("interpret works for function with return", {
  tokens <- scan_tokens('fun sayHi(first, last) {return "Hi, " + first + " " + last + "!";} print sayHi("bobby", "z");')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "Hi, bobby z!")
})

test_that("interpret works with closure", {
  tokens <- scan_tokens('fun makeCounter() {var i = 0; fun count() {i = i + 1; print i;} return count;} var counter = makeCounter(); counter(); counter();')
  p <- parse_tokens(tokens)
  expect_output(interpret(p), "1\\n2")
})
