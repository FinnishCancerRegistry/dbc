# this script was generated automatically. do not edit by hand!



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "general",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_input_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "input",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_user_input_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "user_input",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_prod_input_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "prod_input",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_dev_input_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "dev_input",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_prod_output_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "prod_output",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_dev_output_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "dev_output",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_prod_interim_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "prod_interim",
    raise_error_call = call
  )
}


# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
assert_dev_interim_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- eval(substitute(
    report_is(x = x, x_nm = x_nm, call = call),
    list(x = substitute(x))
  ))
  report_df[["call"]][[1L]] <- call
  report_to_assertion(
    report_df,
    assertion_type = "dev_interim",
    raise_error_call = call
  )
}

