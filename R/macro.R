#' Defines a macro
#'
#' A macro is a function that takes code and returns code.
#'
#' @param fun the function
#'
#' @export
defmacro <- function(fun) {
  structure(list(expand = fun), class = "defmacro_macro")
}

#' Expands code within a function
#' @param fun the function
#' @param envir the environment where to look for macros and other symbols
#'
#' @export
expand_function <- function(fun, envir = parent.frame()) {
  stopifnot(is.function(fun))
  code <- expand_code(body(fun), envir)
  body(fun) <- code
  fun
}

#' The onLoad hook
#' @param pkg_name the package name
#' @export
onload <- function(pkg_name) {
  envir <- getNamespace(pkg_name)
  macro_envir <- new.env(parent = baseenv())
  for (name in names(envir)) {
    if ("defmacro_macro" %in% class(envir[[name]])) {
      macro_envir[[name]] <- envir[[name]]
    }
  }
  for (name in names(envir)) {
    if (is.function(envir[[name]])) {
      assign(name,
        expand_function(envir[[name]], macro_envir),
        envir = envir
      )
    }
  }
}


expand_code <- function(code, macro_environment) {
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    ast <- if (is.null(element$ast)) get_ast_value(path) else element$ast
    if (is.call(ast)) {
      fun_name <- paste0(deparse(ast[[1L]]), collapse = "")
      if (!is.null(macro_environment[[fun_name]])) {
        macro <- macro_environment[[fun_name]]
        args <- lapply(seq_len(length(ast) - 1L) + 1, function(i) {
          bquote(quote(.(ast[[i]])))
        })
        result <- do.call(macro$expand, args)
        inplace_update_ast(path, result)
        if (!is.null(result)) {
          push(list(ast = result, path = path))
        }
      } else {
        for (i in seq_len(length(ast))) {
          if (i > 1L) {
            push(list(ast = ast[[i]], path = c(path, i)))
          }
        }
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
