

#' @title Argument Handlers
#' @description
#' Functions which handle arguments of check functions.
#' @name argument_handlers

#' @rdname argument_handlers
#' @export
#' @template arg_call
#' @param env `[NULL, environment]` (no default)
#'
#' - `environment`: use this environment
#' - `NULL`: use `parent.frame(2)`
#'
#' When `call` is `NULL`, it will be guessed to be
#' `eval(quote(match.call()), envir = env)`.
handle_arg_call <- function(call = NULL, env = NULL) {
  raise_internal_error_if_not(
    is.language(call) || is.null(call),
    is.environment(env) || i.null(env)
  )
  if (is.language(call)) {
    return(call)
  }
  if (is.null(env)) {
    env <- parent.frame(2L)
  }
  if (is.null(call)) {
    env_list <- unique(list(env, parent.frame(2L), parent.frame(1L)))
    for (env_list_elem in env_list) {
      call <- tryCatch(
        eval(quote(match.call()), envir = env_list_elem),
        error = function(e) e
      )
      if (is.language(call) && !identical(call, quote(match.call()))) {
        break()
      }
    }
    if (!is.language(call) || identical(call, quote(match.call()))) {
      call <- NULL
    }
  }
  return(call)
}



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
  this_call <- match.call()
  test_exprs <- as.list(this_call)[-1L]
  test_results <- list(...)
  lapply(seq_along(test_results), function(i) {
    test_result <- test_results[[i]]
    if (!is.logical(test_result)) {
      err <- simpleError(
        message = paste0(
          "internal error: test ", deparse(test_exprs[[i]]),
          " did not evaluate to logical values; ",
          "result had class(es) ", deparse(class(test_result))
        ),
        call = this_call
      )
      stop(err)
    } else if (!all(test_result)) {
      err <- simpleError(
        message = paste0(
          "internal error: not all were TRUE: ", deparse(test_exprs[[i]])
        ),
        call = this_call
      )
      stop(err)
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



settings_env <- new.env(parent = emptyenv())
settings_env[["in_dev_mode"]] <- FALSE
#' @title Development Mode
#' @description
#' Set and get development mode for **dbc** functions.
#' @param value `[logical]` (mandatory, no default)
#'
#' if set to `TRUE`, "dev" assertions will be evaluated; else they won't be
#' @name dev_mode
NULL

#' @export
#' @rdname dev_mode
set_dev_mode <- function(value) {
  stopifnot(
    length(value) == 1L,
    identical(value, TRUE) || identical(value, FALSE)
  )
  settings_env[["in_dev_mode"]] <- value
}

#' @export
#' @rdname dev_mode
get_dev_mode <- function() {
  identical(settings_env[["in_dev_mode"]], TRUE)
}





.__ERROR_DATA_ENV <- new.env(parent = emptyenv())
.__ERROR_DATA_ENV[["data"]] <- list()

get_error_dataset <- function() {
  .__ERROR_DATA_ENV[["data"]]
}
add_error_data <- function(data) {
  raise_internal_error_if_not(
    inherits(data, "list"),
    c("sys.calls", "call", "msg") %in% names(data)
  )
  .__ERROR_DATA_ENV[["data"]] <- c(list(data), .__ERROR_DATA_ENV[["data"]])
  return(invisible(NULL))
}
#' @title Error Data
#' @description Retrieve data about previous errors.
#' @name error_data
#' @return
#' Both `get_error_data` and `get_newest_error_data` return a list containing
#' elements
#'
#' - `msg`: `character`; error message
#' - `call`: `call`; call where error was raised
#' - `sys.calls`: output of `[base::sys.calls]` in the context of where the
#'   error was generated

#' @param n `[integer]` (optional, default `1L`)
#'
#' position of error in list of errors in chronological order recorded during
#' this R session; `n = 1L` is the position of the newest error
#' @rdname error_data
#' @export
get_error_data <- function(n = 1L) {
  raise_internal_error_if_not(
    length(n) == 1L,
    n %% 1L == 0L,
    n > 0L
  )
  ds <- get_error_dataset()
  if (length(ds) == 0L) {
    stop("No errors recorded, cannot return anything")
  } else if (n > length(ds)) {
    stop("n = ", n, " is too large: only ", length(ds), " errors have been ",
         "recorded")
  }
  ds[[n]]
}

#' @rdname error_data
#' @export
get_newest_error_data <- function() {
  get_error_data(n = 1L)
}









