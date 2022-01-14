
#' @rdname assertions
#' @export
#' @param funs `[character, list]` (mandatory, no default)
#'
#' report functions that return a report (data.frame);
#' - `character`: names of functions that can be found by `[match.fun]`
#' - `list`: list of functions
report_is_one_of <- function(x, x_nm = NULL, call = NULL, funs) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )

  funs <- lapply(funs, match.fun)
  report_df <- do.call(rbind, lapply(funs, function(fun) {
    report_df <- fun(x = x, x_nm = x_nm, call = call)
    report_df[["all_pass"]] <- all(report_df[["pass"]] %in% TRUE)
    total_report_df <- data.frame(
      test = paste0(report_df[["test"]], collapse = " & "),
      error = paste0(report_df[["error"]], collapse = "; "),
      pass = all(report_df[["pass"]] %in% TRUE),
      n_fail = max(report_df[["n_fail"]], na.rm = TRUE),
      wh_fail = list_union(report_df[["wh_fail"]]),
      message = paste0(report_df[["message"]], collapse = "; ")
    )
    if (any(!is.na(total_report_df[["wh_fail"]]))) {
      total_report_df[["wh_fail"]] <- setdiff(total_report_df[["wh_fail"]],
                                              NA_integer_)
    }
    return(total_report_df)
  }), quote = TRUE)

  return(report_df)
}


assert_is_one_of__ <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL,
  assertion_type = "general"
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  call <- dbc::handle_arg_call(call)

  fun_list <- lapply(funs, match.fun)
  if (is.character(funs)) {
    names(fun_list) <- funs
  } else {
    names(fun_list) <- paste0("fun_", seq_along(fun_list))
  }
  report_df <- dbc::report_is_one_of(
    x = x, x_nm = x_nm, funs = funs, call = call
  )

  if (!any(report_df[["pass"]])) {
    msgs <- paste0("test \"", report_df[["test"]], "\" failed: ",
                   report_df[["message"]])
    msg <- paste0(
      "    * ", msgs,
      collapse = "\n"
    )
    msg <- paste0("None of the following assertions passed:\n", msg)
    emit_error <- identical(get_dev_mode(), TRUE) &&
      assertion_type %in% dev_assertion_types()
    emit_error <- emit_error || !assertion_type %in% dev_assertion_types()
    if (emit_error) {
      stop(msg)
    }
  }

  return(invisible(NULL))
}

#' @rdname assertions
#' @export
assert_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assert_is_one_of__(
    x = x, x_nm = x_nm, funs = funs, call = call,
    assertion_type = "general"
  )
}


#' @rdname assertions
#' @export
assert_prod_input_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "prod_input"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}
#' @rdname assertions
#' @export
assert_prod_output_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "prod_output"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}
#' @rdname assertions
#' @export
assert_prod_interim_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "prod_interim"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}
#' @rdname assertions
#' @export
assert_user_input_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "user_input"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}
#' @rdname assertions
#' @export
assert_dev_input_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "dev_input"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}
#' @rdname assertions
#' @export
assert_dev_interim_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "dev_interim"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}
#' @rdname assertions
#' @export
assert_dev_output_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "dev_output"
  assert_is_one_of__(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}



