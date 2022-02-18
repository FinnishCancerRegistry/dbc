
#' @title Function Calls
#' @description
#' Retrieve upsteam function calls.
#' @name function_calls
#' @examples
#' tf1 <- function(tf1_arg = 1) {
#'   get_current_call()
#' }
#' stopifnot(
#'   identical(tf1(), quote(tf1()))
#' )
#' tf2 <- function(tf2_arg = 2) {
#'   get_parent_call()
#' }
#' tf3 <- function(tf3_arg = 3) {
#'   tf2()
#' }
#' tf4 <- function(tf4_arg = 4) {
#'   get_parent_call()
#' }
#' \donttest{
#' stopifnot(
#'   identical(tf3(), quote(tf3())),
#'   inherits(tryCatch(get_current_call(), error = function(e) e), "error"),
#'   inherits(tryCatch(get_parent_call(), error = function(e) e), "error"),
#'   inherits(tryCatch(tf4(), error = function(e) e), "error")
#' )
#' }
NULL

#' @rdname function_calls
#' @export
#' @section Functions:
#' - `get_current_call`: when called inside another function, gives the call
#'   of that function. See **Examples**.
get_current_call <- function() {
  n <- 1L
  call <- tryCatch(
    match.call(
      definition = sys.function(sys.parent(n)),
      call = sys.call(sys.parent(n)),
      envir = parent.frame(n)
    ),
    error = function(e) e
  )
  if (inherits(call, "error") || identical(call, quote(get_current_call()))) {
    stop("Could not determine crreunt call. Was get_current_call not ",
         "called in another function?")
  }
  call
}

#' @rdname function_calls
#' @export
#' @section Functions:
#' - `get_parent_call`: when called inside another function, gives the call
#'   of the function calling that one. See **Examples**.
get_parent_call <- function() {
  n <- 2L
  call <- tryCatch(
    match.call(
      definition = sys.function(sys.parent(n)),
      call = sys.call(sys.parent(n)),
      envir = parent.frame(n)
    ),
    error = function(e) e
  )
  if (inherits(call, "error") || identical(call, quote(get_parent_call()))) {
    stop("Could not determine parent call. get_parent_call can only be used ",
         "in a function called by another.")
  }
  call
}

#' @rdname function_calls
#' @export
#' @section Functions:
#' - `get_nth_call`: when called inside another function, gives the `n`'th
#'   parent call of that function.
#' @param n `[integer]` (default `1L`)
#'
#' Integer larger than zero. `1L` gives the call of the function which calls
#' `get_nth_call`.
get_nth_call <- function(n = 1L) {
  call <- tryCatch(
    match.call(
      definition = sys.function(sys.parent(n)),
      call = sys.call(sys.parent(n)),
      envir = parent.frame(n)
    ),
    error = function(e) e
  )
  if (inherits(call, "error") || identical(call, quote(get_nth_call()))) {
    stop("Could not determine nth call. get_nth_call can only be used ",
         "in a function.")
  }
  call
}


#' @title Argument Handlers
#' @description
#' Functions which handle arguments of check functions.
#' @name argument_handlers

#' @rdname argument_handlers
#' @export
#' @template arg_call
#' @param env `[NULL, environment, other]` (optional, default `NULL`)
#'
#' Environment where `call` / `x.nm` is inferred in using `[substitute]`
#' (`substitute(call, env)` / `substitute(x, env = env)`), if
#' `call` is `NULL`.
#' @section Functions:
#' - `dbc::handle_arg_call` is used internally in other functions
#'   to guess `call` which is to be reported if there is a problem
#' @return
#' - `dbc::handle_arg_call`: returns an R `language` object, or `NULL` upon
#'   failure to guess the call
handle_arg_call <- function(call = NULL, env = NULL) {
  raise_internal_error_if_not(
    is.environment(env) || is.null(env),
    is.language(call) || is.null(call)
  )
  if (is.language(call)) {
    return(call)
  }
  if (is.null(env)) {
    env <- parent.frame(2L)
  }
  if (is.null(call)) {
    call_inferrer <- quote(match.call())
    call <- tryCatch(
      eval(call_inferrer, envir = env),
      error = function(e) e
    )
    is_bad_call <- inherits(call, "error") || identical(call, call_inferrer)
    if (is_bad_call) {
      for (i in 2:1) {
        call <- tryCatch(
          eval(call_inferrer, envir = parent.frame(i)),
          error = function(e) e
        )
        is_bad_call <- inherits(call, "error") || identical(call, call_inferrer)
        if (!is_bad_call) {
          break()
        }
      }
      if (is_bad_call) {
        warning("Could not infer in which call an assertion was used, ",
                "\"could_not_infer_call\" will be reported as the call in ",
                "any error message")
        call <- parse(text = "could_not_infer_call")[[1L]]
      }
    }
  }
  return(call)
}

#' @rdname argument_handlers
#' @template arg_x_nm
#' @export
#' @section Functions:
#' - `dbc::handle_arg_x_nm` is used internally in other functions
#'   to guess the name of the object passed to argument `x` when it is not
#'   supplied explicitly. It uses object `x` in its caller environment
#'   to guess `x_nm` if it is `NULL`.
#' @return
#' - `dbc::handle_arg_x_nm`: always returns a character vector of length 1
handle_arg_x_nm <- function(x_nm, env = NULL) {
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  raise_internal_error_if_not(
    (is.character(x_nm) && length(x_nm) == 1L && !is.na(x_nm)) || is.null(x_nm),
    is.environment(env) || is.null(env)
  )
  if (is.null(x_nm)) {
    x_nm <- paste0(
      deparse(substitute(x, env = env)),
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



list_union <- function(x) {
  stopifnot(inherits(x, "list"))
  out <- NULL
  fun_env <- environment()
  invisible(lapply(seq_along(x), function(i) {
    fun_env[["out"]] <- union(out, x[[i]])
    NULL
  }))
  return(out)
}



