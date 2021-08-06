#' @export
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
  #bytes <- readBin(path, what = "raw")
  #obj <- run.lox(obj, bytes)
  #src <- paste(readLines(path), collapse = "\n")
  src <- readChar(path, file.info(path)$size)
  obj <- run.lox(obj, src)
  if (obj$had_error) stop()
}

run_prompt <- function(obj) UseMethod("run_prompt")
run_prompt.lox <- function(obj) {
  is_interactive <- interactive()
  while(TRUE) {
    if (is_interactive) {
      line <- readline("(lox) > ")
    }  else {
      #cat("(lox) > ")
      #line <- readLines(n = 1)
      stop("rlox prompt must be run in interactive mode")
    }

    if (is.null(line) || length(line) < 1) break
    obj <- tryCatch(
      error = function(cnd) {
        msg <- conditionMessage(cnd)
        cat(msg, "\n")
        obj$had_error <- TRUE
        obj
      },
      run.lox(obj, line)
    )

    obj$had_error <- FALSE
  }
}

run <- function(obj, line) UseMethod("run")
run.lox <- function(obj, line) {
  # scan tokens
  tokens <- scan_tokens(line)
  p <- parse_expression(tokens)
  expr <- p$expr

  # print tokens
  #for (token in tokens) {
  #  cat(format(token), "\n")
  #}
  print(expr)

  # return object
  obj
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

lox_error <- function(line, msg) {
  lox_report(line, msg)
}

lox_report <- function(line, msg, where = "") {
  msg <- sprintf("[line %i] %s %s", line, where, msg)
  stop(msg, call. = FALSE)
}

lox_error2 <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(message = message, call = call, ...),
    class = c(.subclass, "error", "condition")
    )
  stop(err$message, call. = FALSE)
}
