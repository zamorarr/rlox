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
  if (obj$had_error) stop() # exit 65
  # if(obj$had_runtime_error) quit(save = "no", status = 70)
}

run_prompt <- function(obj) UseMethod("run_prompt")
run_prompt.lox <- function(obj) {
  is_interactive <- interactive()
  if (!is_interactive) stop("rlox prompt must be run in interactive mode")
  while(TRUE) {
    #cat("(lox) > ")
    #line <- readLines(n = 1)
    # read line
    line <- readline("(lox) > ")

    # check if line is blank (to indicate a quit)
    if (is.null(line) || length(line) < 1) break

    # try and run line
    obj <- tryCatch(
      # statement
      run.lox(obj, line),

      # runtime errors
      runtime_error = function(cnd) {
        message(conditionMessage(cnd))
        obj$had_error <- TRUE
        obj
      },

      # other errors
      error = function(cnd) {
        message(conditionMessage(cnd))
        obj$had_error <- TRUE
        obj
      }
    )

    # reset error flag (so interpreter can continue)
    obj$had_error <- FALSE
  }
}

run <- function(obj, line) UseMethod("run")
run.lox <- function(obj, line) {
  # scan tokens
  tokens <- scan_tokens(line)
  statements <- parse_lox(tokens)
  interpret(statements)

  # return object
  obj
}

