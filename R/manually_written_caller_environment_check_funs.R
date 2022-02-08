
#' @title Caller Environment Checks
#' @description Functions to check caller environment of a function.
#' @param x `[NULL, environment]` (default `NULL`)
#'
#' - `environment`: this environment is compared against `[globalenv]` output
#' - `NULL`: `x` is taken to be `parent.frame(2L)` in the check function,
#'   which should be the caller environment of the function you are trying
#'   to check; See **Examples**
#' @param x_nm `[NULL, character]` (default `NULL`)
#'
#' - `NULL`: The name of the function you are checking is guessed from
#'   `dbc::get_parent_call()` output
#' - `character`: This should be the name of the function you are checking.
#'
#' @template arg_call
#' @name caller_environment_checks

#' @rdname caller_environment_checks
#' @export
report_function_caller_environment_is_not_global_environment <- function(
  x = NULL,
  x_nm = NULL,
  call = NULL
) {
  if (is.null(x)) {
    x <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    parent_call <- dbc::get_parent_call()
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- dbc::handle_arg_call(call)
  dbc::expressions_to_report(
    expressions = paste0(
      "!identical(x, globalenv())"
    ),
    fail_messages = paste0(
      "Looks like function ", deparse(x_nm), " was called in the global ",
      "environment; ", deparse(x_nm), " is intended to be only called in ",
      "other environments, such as in other functions."
    ),
    pass_messages = paste0(
      "Looks like function ", deparse(x_nm), " was not called in the global ",
      "environment, as expected."
    ),
    call = call
  )
}

#' @rdname caller_environment_checks
#' @export
report_function_caller_environment_is_global_environment <- function(
  x = NULL,
  x_nm = NULL,
  call = NULL
) {
  if (is.null(x)) {
    x <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    parent_call <- dbc::get_parent_call()
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- dbc::handle_arg_call(call)
  dbc::expressions_to_report(
    expressions = c(
      "is.environment(x)",
      "identical(x, globalenv())"
    ),
    fail_messages = c(
      "Object ${deparse(x)} is not an environment.",
      paste0(
        "Looks like function ", deparse(x_nm), " was not called in the global ",
        "environment; ", deparse(x_nm), " is intended to be only called in ",
        "the global environment, and not in other environments, such as in ",
        "other functions."
      )
    ),
    pass_messages = c(
      "Object ${deparse(x)} is an environment.",
      paste0(
        "Looks like function ", deparse(x_nm), " was called in the global ",
        "environment, as expected."
      )
    ),
    call = call
  )
}

#' @rdname caller_environment_checks
#' @export
assert_function_caller_environment_is_not_global_environment <- function(
  x = NULL,
  x_nm = NULL,
  call = NULL
) {
  if (is.null(x)) {
    x <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    parent_call <- dbc::get_parent_call()
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- dbc::handle_arg_call(call)
  report_df <- report_function_caller_environment_is_not_global_environment(
    x = x, x_nm = x_nm, call = call
  )
  dbc::report_to_assertion(
    report_df = report_df,
    assertion_type = "general",
    raise_error_call = call
  )
}

#' @rdname caller_environment_checks
#' @export
assert_function_caller_environment_is_global_environment <- function(
  x = NULL,
  x_nm = NULL,
  call = NULL
) {
  if (is.null(x)) {
    x <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    parent_call <- dbc::get_parent_call()
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- dbc::handle_arg_call(call)
  report_df <- report_function_caller_environment_is_global_environment(
    x = x, x_nm = x_nm, call = call
  )
  dbc::report_to_assertion(
    report_df = report_df,
    assertion_type = "general",
    raise_error_call = call
  )
}


#' @rdname caller_environment_checks
#' @export
test_function_caller_environment_is_not_global_environment <- function(
  x = NULL,
  x_nm = NULL,
  call = NULL
) {
  if (is.null(x)) {
    x <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    parent_call <- dbc::get_parent_call()
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- dbc::handle_arg_call(call)
  report_df <- report_function_caller_environment_is_not_global_environment(
    x = x,
    x_nm = x_nm,
    call = call
  )
  return(all(report_df[["pass"]]))
}

#' @rdname caller_environment_checks
#' @export
test_function_caller_environment_is_global_environment <- function(
  x = NULL,
  x_nm = NULL,
  call = NULL
) {
  if (is.null(x)) {
    x <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    parent_call <- dbc::get_parent_call()
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- dbc::handle_arg_call(call)
  report_df <- report_function_caller_environment_is_global_environment(
    x = x,
    x_nm = x_nm,
    call = call
  )
  return(all(report_df[["pass"]]))
}


