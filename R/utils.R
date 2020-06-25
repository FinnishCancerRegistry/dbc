




#' @title Utilities
#' @description
#' Utility functions, intended primarily for internal use.
#' @name utilities
NULL

#' @rdname utilities
#' @param x_nm `[NULL, character]` (mandatory, no default)
#'
#' - `character`: return input as-is
#' - `NULL`: it is assumed that this function is called within another function,
#'   and essentially `deparse(substitute(x))` is returned
#' @export
#' @details
#' - `handle_x_nm_arg` is used internally in `report_` and `assert_` functions
#'   to guess the name of the object passed to argument `x` when it is not
#'   supplied explicitly.
#' @return
#' - `handle_x_nm_arg`: always returns a character vector of length 1
handle_x_nm_arg <- function(x_nm) {
  stopifnot(
    (is.character(x_nm) && length(x_nm) == 1L && !is.na(x_nm)) || is.null(x_nm)
  )
  if (is.null(x_nm)) {
    x_nm <- paste0(
      deparse(substitute(x, env = parent.frame(1L))),
      collapse = ""
    )
  }
  x_nm
}





raise_internal_error_if_not <- function(...) {
  test_exprs <- substitute(list(...))
  test_results <- list(...)
  lapply(seq_along(test_results), function(i) {
    test_result <- test_results[[i]]
    if (!is.logical(test_result)) {
      stop("test ", deparse(test_exprs[[i]]),
           " did not evaluate to logical values; ",
           "result had class(es) ", deparse(class(test_result)))
    } else if (!all(test_result)) {
      stop("not all were TRUE: ", deparse(test_exprs[[i]]))
    }
  })
  invisible(NULL)
}






call_with_arg_list <- function(
  fun,
  arg_list,
  envir = parent.frame(1L)
) {
  stopifnot(
    inherits(arg_list, "list"),
    is.environment(envir)
  )
  match.fun(fun)
  UseMethod("call_with_arg_list")
}

call_with_arg_list.function <- function(
  fun,
  arg_list,
  envir = parent.frame(1L)
) {
  call_env <- new.env(parent = envir)
  call <- quote(fun())
  for (arg_nm in names(arg_list)) {
    tmp_arg_nm <- paste0("._", arg_nm)
    call[[arg_nm]] <- parse(text = tmp_arg_nm)[[1L]]
    call_env[[tmp_arg_nm]] <- arg_list[[arg_nm]]
  }
  call_env[["fun"]] <- fun
  eval(expr = call, envir = call_env)
}

call_with_arg_list.character <- function(
  fun,
  arg_list,
  envir = parent.frame(1L)
) {
  call_env <- new.env(parent = envir)
  call <- parse(text = paste(fun, "()"))[[1L]]
  for (arg_nm in names(arg_list)) {
    tmp_arg_nm <- paste0("._", arg_nm)
    call[[arg_nm]] <- parse(text = tmp_arg_nm)[[1L]]
    call_env[[tmp_arg_nm]] <- arg_list[[arg_nm]]
  }
  call_env[[fun]] <- match.fun(fun)
  eval(expr = call, envir = call_env)
}


