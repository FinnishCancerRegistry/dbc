
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
#' - `NULL`: The name of the function you are checking is guessed via
#'   `eval(quote(match.call()), parent.frame(1L))`
#' - `character`: This should be the name of the function you are checking.
#'
#' @template arg_call
#' @name caller_environment_checks
#' @examples
#' # for use in other functions only
#' my_fun_ <- function() {
#'   dbc::test_function_caller_environment_is_not_global_environment()
#' }
#' result <- my_fun_()
#' stopifnot(identical(result, FALSE))
#'
#' # for use by the user only
#' my_fun <- function() {
#'   dbc::test_function_caller_environment_is_global_environment()
#' }
#' result <- my_fun()
#' stopifnot(identical(result, TRUE))
#'
#' # for use by the user only
#' my_fun <- function() {
#'   dbc::assert_function_caller_environment_is_global_environment()
#'   my_fun_()
#' }
#' result <- my_fun()
#' stopifnot(identical(result, TRUE))
#'
#' # this would be a mistake
#' my_other_fun <- function() {
#'   my_fun()
#' }
#' result <- tryCatch(my_other_fun(), error = function(e) e)
#' stopifnot(
#'   is.character(result[["message"]]),
#'   grepl("global", result[["message"]])
#' )
#'

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
    parent_call <- eval(quote(match.call()), parent.frame(1L))
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- infer_call(call, parent.frame(1L))
  dbc::tests_to_report(
    tests = paste0(
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
    parent_call <- eval(quote(match.call()), parent.frame(1L))
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- infer_call(call, parent.frame(1L))
  dbc::tests_to_report(
    tests = paste0(
      "identical(x, globalenv())"
    ),
    fail_messages = paste0(
      "Looks like function ", deparse(x_nm), " was not called in the global ",
      "environment; ", deparse(x_nm), " is intended to be only called in ",
      "the global environment, and not in other environments, such as in ",
      "other functions."
    ),
    pass_messages = paste0(
      "Looks like function ", deparse(x_nm), " was called in the global ",
      "environment, as expected."
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
    parent_call <- eval(quote(match.call()), parent.frame(1L))
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- infer_call(call, parent.frame(1L))
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
    parent_call <- eval(quote(match.call()), parent.frame(1L))
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- infer_call(call, parent.frame(1L))
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
    parent_call <- eval(quote(match.call()), parent.frame(1L))
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- infer_call(call, parent.frame(1L))
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
    parent_call <- eval(quote(match.call()), parent.frame(1L))
    x_nm <- deparse(parent_call[[1L]])
  }
  call <- infer_call(call, parent.frame(1L))
  report_df <- report_function_caller_environment_is_global_environment(
    x = x,
    x_nm = x_nm,
    call = call
  )
  return(all(report_df[["pass"]]))
}


