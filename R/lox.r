lox <- function(args = NULL) {
  x <- list(had_error = FALSE)
  obj <- structure(x, class = c("lox", class(x)))

  if (length(args) > 1) {
    cat("Usage: rlox [script]\n")
    #quit(save = "no", status = 64)
    stop()
  } else if (length(args) == 1) {
    run_file(obj, args[1])
  } else {
    run_prompt(obj)
  }
}

run_file <- function(obj, path) UseMethod("run_file")
run_file.lox <- function(obj, path) {
  bytes <- readBin(path, what = "raw")
  obj <- run.lox(obj, bytes)

  if (obj$had_error) stop()
}

run_prompt <- function(obj) UseMethod("run_prompt")
run_prompt.lox <- function(obj) {
  while(TRUE) {
    cat("(lox) > ")
    line <- readLines(n = 1)
    if (is.null(line) || length(line) < 1 || nchar(line) < 1) break
    obj <- run.lox(obj, line)
    obj$had_error <- FALSE
  }
}

run <- function(obj, line) UseMethod("run")
run.lox <- function(obj, line) {
  tokens <- scan_tokens(line)
  for (token in tokens) {
    cat(format(token), "\n")
  }
}

error <- function(obj, line, msg) UseMethod("error")
error.lox <- function(obj, line, msg) {
  report.lox(obj, line, "", msg)
}

report <- function(obj, line, where, msg) UseMethod("report")
report.lox <- function(obj, line, where, msg) {
  cat(sprintf("[line %i] Error %s: %s", line, where, msg))
  lox$had_error <- TRUE
  lox
}
