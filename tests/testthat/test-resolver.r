test_that("resolve errors when trying to use variable in initializer", {
  statements <- parse_tokens(scan_tokens("{var x = 2*x;}"))
  expect_error(resolve_statements(statements))
})

test_that("resolver works", {
  s <- 'var a = "global"; {fun showA() {print a;} showA(); var a = "block"; showA();}'
  statements <- parse_tokens(scan_tokens(s))
  locals <- resolve_statements(statements)
  expect_identical(locals$get("11"), 1L) # showA() call
  expect_identical(locals$get("16"), 1L) # showA() call
})

test_that("resolver does not allow declaring variable already declared in scope", {
  s <- 'fun bad() {var a = "first"; var a = "second"; print a;} bad();'
  tokens <- scan_tokens(s)
  statements <- parse_tokens(tokens)
  expect_error(resolve_statements(statements))
})

test_that("cannot return at top level code", {
  s <- 'return "at top level";'
  tokens <- scan_tokens(s)
  statements <- parse_tokens(tokens)
  expect_error(resolve_statements(statements))
})
