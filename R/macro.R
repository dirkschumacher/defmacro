#' Defines a macro
#'
#' A macro is a function that takes code and returns code.
#'
#' @param fun the function
#'
#' @export
defmacro <- function(fun) {
  class(fun) <- c("defmacro_macro", class(fun))
  fun
}

#' Checks if an object is a macro
#'
#' @param fun any object
#'
#' @export
is_macro <- function(fun) {
  is.function(fun) && inherits(fun, "defmacro_macro")
}

#' Expands code within a function
#'
#' @param fun the function
#' @param envir the environment where to look for macros and other symbols
#'
#' @export
expand_function <- function(fun, envir = parent.frame()) {
  stopifnot(is.function(fun))
  old_classes <- class(fun)
  code <- expand_code(body(fun), envir)
  body(fun) <- code
  if (length(old_classes) > 1) {
    class(fun) <- old_classes
  }
  fun
}

#' The onLoad hook
#'
#' It looks in the package code for functions and recursively
#' expands all macros. After expansion it compiles the function
#' to bytecode using the \code{compiler} package.
#'
#' @param envir an environment in which the macro expansion is being done.
#'
#' @export
onload <- function(envir = topenv(parent.frame())) {
  names <- ls(envir, all.names = FALSE, sorted = FALSE)
  for (name in names) {
    obj <- get0(name, envir = envir)
    if (is.function(obj)) {
      assign(
        name,
        compiler::cmpfun(expand_function(obj, envir)),
        envir = envir
      )
    }
  }
}

expand_code <- function(code, macro_environment) {
  if (missing(code) || !is.call(code)) {
    return(code)
  }
  fun_name <- paste0(deparse(code[[1]]), collapse = "")
  fun <- get0(fun_name, envir = macro_environment)
  if (is_macro(fun)) {
    quoted_args <- lapply(seq_len(length(code) - 1) + 1, function(i) {
      bquote(quote(.(code[[i]])))
    })
    return(do.call(fun, quoted_args))
  }
  for (i in seq_len(length(code))) {
    if (i > 1L) {
      code[[i]] <- expand_code(code[[i]], macro_environment)
    }
  }
  code
}
