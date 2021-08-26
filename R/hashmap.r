#' HashMap
#' @export
HashMap <- R6::R6Class(
  "HashMap",
  public = list(
    #' @description initialize constructor
    initialize = function() {
      private$.data <- new.env(parent = emptyenv())
    },

    #' @description put add key/value to hashmap
    #' @param key any hashable R object
    #' @param value any R object
    put = function(key, value) {
      assign(key, value, envir = private$.data, inherits = FALSE)
    },

    #' @description get value given key
    #' @param key any hashable R object
    get = function(key) {
      get(key, envir = private$.data, inherits = FALSE)
    },

    #' @description has check if key exists
    #' @param key any hashable R object
    has = function(key) {
      exists(key, envir = private$.data, inherits = FALSE)
    },

    #' @description rm remove key
    #' @param key any hashable R object
    rm = function(key) {
      rm(list = key, envir = private$.data)
    },

    #' @description is_empty check if hashmap is empty
    is_empty = function() {
      identical(self$size, 0L)
    },

    #' @description print show hashmap
    print = function() {
      cat(sprintf("<HashMap: %i items>\n", self$size))
    }
  ),
  private = list(
    .data = list()
  ),
  active = list(
    #' @field keys list of keys
    keys = function(value) {
      if (missing(value)) names(private$.data)
      else stop("`keys` is read only", call. = FALSE)
    },

    #' @field size number of keys
    size = function(value) {
      if (missing(value)) length(private$.data)
      else stop("`size` is read only", call. = FALSE)
    }
  )
)
