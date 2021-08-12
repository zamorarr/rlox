#!/usr/bin/Rscript --no-save --no-restore --quiet
args <- commandArgs(TRUE)

if (length(args) != 1) {
  cat("Usage: rlox [script]\n")
  quit(save = "no", status = 64)
}

src <- args[1]
rlox::lox(src)

# try this one for a REPL if no arguments passed?
#!/usr/bin/R --no-save --no-restore --quiet --interactive

