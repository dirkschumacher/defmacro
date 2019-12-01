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
#' @param pkg_name the package name
#' @export
onload <- function(pkg_name) {
  envir <- getNamespace(pkg_name)
  for (name in names(envir)) {
    if (is.function(envir[[name]])) {
      assign(
        name,
        compiler::cmpfun(expand_function(envir[[name]], envir)),
        envir = envir
      )
    }
  }
}

expand_code <- function(code, macro_environment) {
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    ast <- if (is.null(element$ast)) {
      get_ast_value(path)
    } else {
      element$ast
    }
    if (!is.call(ast)) {
      return()
    }
    fun_name <- paste0(deparse(ast[[1L]]), collapse = "")
    fun <- get0(fun_name, envir = macro_environment)
    if (is_macro(fun)) {
      result <- exec(fun, !!!as.list(ast)[-1])
      inplace_update_ast(path, result)
      if (!is.null(result)) {
        push(list(ast = result, path = path))
      }
      return()
    }
    for (i in seq_len(length(ast))) {
      if (i > 1L) {
        push(list(ast = ast[[i]], path = c(path, i)))
      }
    }
  }
  ast_walker(code, on_element)
}


# walks around a central AST and changes it
# it does not use recursion to avoid memory issues
ast_walker <- function(ast, on_element) {
  # rather just use the path to query ast on demand
  stack_data <- list()
  push <- function(x) stack_data <<- list(x, stack_data)
  push(list(ast = ast, path = integer()))
  get_ast_value <- function(path) {
    if (length(path) > 0L) {
      ast[[path]]
    } else {
      ast
    }
  }
  inplace_update_ast <- function(path, value) {
    # update the ast in place
    if (length(path) > 0L) {
      ast[[path]] <<- value
    } else {
      ast <<- value
    }
  }
  while (length(stack_data) > 0L) {
    element <- stack_data[[1L]]
    stack_data <- stack_data[[2L]]
    on_element(push, inplace_update_ast, get_ast_value, element)
  }
  ast
}
