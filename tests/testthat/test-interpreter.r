test_that("interpreter throws error for undefined variable", {
  tokens <- scan_tokens("d;")
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_error(interpret(statements, locals))
})

test_that("interpreter works for if statement", {
  x <- 'var a = 3; var b = 4;if (a > b) print "a bigger"; else print "a not bigger";'
  tokens <- scan_tokens(x)
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "a not bigger")
})


test_that("interpreter works for OR expression", {
  tokens <- scan_tokens('print nil or "yes";')
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "yes")

  tokens <- scan_tokens('print true or false;')
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "true")
})

test_that("interpreter works for AND expression", {
  tokens <- scan_tokens('print nil and "yes";')
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "nil")

  tokens <- scan_tokens('print 1 < 2 and 10 > 3;')
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "true")
})

test_that("interpret works for WHILE statement", {
  tokens <- scan_tokens('var i = 1; while (i <= 3) {print i; i = i + 1;}')
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "1\\n2\\n3")
})

test_that("interpret works for function with return", {
  x <- 'fun sayHi(first, last) {return "Hi, " + first + " " + last + "!";} print sayHi("bobby", "z");'
  tokens <- scan_tokens(x)
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "Hi, bobby z!")
})

test_that("interpret works with closure", {
  tokens <- scan_tokens('fun makeCounter() {var i = 0; fun count() {i = i + 1; print i;} return count;} var counter = makeCounter(); counter(); counter();')
  statements <- parse_tokens(tokens)
  locals <- resolve_statements(statements)
  expect_output(interpret(statements, locals), "1\\n2")
})
