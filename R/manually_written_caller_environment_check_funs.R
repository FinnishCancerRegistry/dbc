
#' @title Caller Environment Checks
#' @description Functions to check caller environment of a function.
#' @param x `[environment]` (mandatory, no default)
#'
#' This should be the caller environment of a function. See **Examples** for
#' more information.
#' @template arg_x_nm
#' @template arg_call
#' @name caller_environment_checks
#' @examples
#' # for use in other functions only
#' my_fun_ <- function() {
#'   caller_env <- parent.frame(1)
#'   dbc::test_function_caller_environment_is_not_global_environment(
#'     x = caller_env,
#'     x_nm = "my_fun_"
#'   )
#' }
#' result <- my_fun_()
#' stopifnot(identical(result, FALSE))
#'
#' # for use by the user only
#' my_fun <- function() {
#'   caller_env <- parent.frame(1)
#'   dbc::test_function_caller_environment_is_global_environment(
#'     x = caller_env,
#'     x_nm = "my_fun"
#'   )
#' }
#' result <- my_fun()
#' stopifnot(identical(result, TRUE))
#'
#' # for use by the user only
#' my_fun <- function() {
#'   caller_env <- parent.frame(1)
#'   dbc::assert_function_caller_environment_is_global_environment(
#'     x = caller_env,
#'     x_nm = "my_fun"
#'   )
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
  x,
  x_nm,
  call = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
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
  x,
  x_nm,
  call = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
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
  x,
  x_nm,
  call = NULL
) {
  caller_env <- parent.frame(1L)
  call <- infer_call(call, caller_env)
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
  x,
  x_nm,
  call = NULL
) {
  caller_env <- parent.frame(1L)
  call <- infer_call(call, caller_env)
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
  x,
  x_nm,
  call = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  call <- infer_call(call = call, env = parent.frame(1L))
  if (is.null(call)) {
    call <- match.call()
  }
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
  x,
  x_nm,
  call = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  call <- infer_call(call = call, env = parent.frame(1L))
  if (is.null(call)) {
    call <- match.call()
  }
  report_df <- report_function_caller_environment_is_global_environment(
    x = x,
    x_nm = x_nm,
    call = call
  )
  return(all(report_df[["pass"]]))
}


