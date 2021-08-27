test_that("resolve errors when trying to use variable in initializer", {
  statements <- parse_tokens(scan_tokens("{var x = 2*x;}"))
  expect_error(resolve_statements(statements))
})

test_that("resolver works", {
  s <- 'var a = "global"; {var a = "block1"; fun showA() {print a;} showA(); var a = "block2"; showA();}'
  statements <- parse_tokens(scan_tokens(s))
  locals <- resolve_statements(statements)
  expect_identical(locals$get("10"), 2L) # print a; refers to once scope above
  expect_identical(locals$get("13"), 1L) # showA() call
  expect_identical(locals$get("18"), 1L) # showA() call
})
