match_pattern <- function(x, pattern) {
  # look for match
  m <- regexpr(pattern, x, perl = TRUE)

  # if no match
  if (m < 1) {
    # use zero instead of negative one to indicate no match
    # so we can use in if statements. also keep other attributes
    m[] <- 0L
  }

  # return match
  m
}

match_left_paren <- function(x) match_pattern(x, "^\\(")
match_right_paren <- function(x) match_pattern(x, "^\\)")
match_left_brace <- function(x) match_pattern(x, "^{")
match_right_brace <- function(x) match_pattern(x, "^}")
match_comma <- function(x) match_pattern(x, "^,")
match_minus <- function(x) match_pattern(x, "^-")
match_plus <- function(x) match_pattern(x, "^\\+")
match_semicolon <- function(x) match_pattern(x, "^;")
match_star <- function(x) match_pattern(x, "^\\*")
match_bang <- function(x) match_pattern(x, "^!(?!=)")
match_bang_equal <- function(x) match_pattern(x, "^!=")
match_equal <- function(x) match_pattern(x, "^=(?!=)")
match_equal_equal <- function(x) match_pattern(x, "^==")
match_less <- function(x) match_pattern(x, "^<(?!=)")
match_less_equal <- function(x) match_pattern(x, "^<=")
match_greater <- function(x) match_pattern(x, "^>(?!=)")
match_greater_equal <- function(x) match_pattern(x, "^>=")
match_slash <- function(x) match_pattern(x, "^/(?!=/)")
match_number <- function(x) match_pattern(x, "^[0-9]+\\.?[0-9]*")
match_whitespace <- function(x) match_pattern(x, "^[^\\S\\n\\r]+")
match_newline <- function(x) match_pattern(x, "^\\r?\\n")
match_comment <- function(x) match_pattern(x, "^//[^\n]*(?:\n|$)")
match_identifier <- function(x) match_pattern(x, "^[[:alnum:]_]+")
match_string <- function(x) match_pattern(x, "^\".*?\"")
