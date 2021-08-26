#' Define a Stack data structure
#' @export
Stack <- R6::R6Class(
  "Stack",
  # public methods and data
  public = list(

    #' @description Push item onto stack
    #' @param item item to push
    push = function(item) {
      private$.data[[self$size + 1L]] <- item
      item
    },

    #' @description Pop item from stack
    pop = function() {
      value <- private$.data[[self$size]]
      private$.data <- private$.data[-self$size]
      value
    },

    #' @description peek get top item from stack
    peek = function() {
      private$.data[[self$size]]
    },

    #' @description get get item i from stack
    #' @param i index
    get = function(i) {
      private$.data[[i]]
    },

    #' @description is_empty check if stack is empty
    is_empty = function() {
      identical(self$size, 0L)
    },

    #' @description print print stack
    #' @param ... extra params (not used)
    print = function(...) {
      cat(sprintf("<Stack: %i items>\n", self$size))
    }
  ),

  # private methods and data
  private = list(
    .data = list()
    ),

  # active bindings
  active = list(
    #' @field size Size of stack
    size = function(value) {
      if (missing(value)) length(private$.data)
      else stop("`size` is read only", call. = FALSE)
    },

    #' @field data Stack data
    data = function(value) {
      if (missing(value)) private$.data
      else stop("`data` is read only", call. = FALSE())
    }
  )
)
