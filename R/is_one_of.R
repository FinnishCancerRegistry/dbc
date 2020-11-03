
#' @rdname assertions
#' @export
#' @param funs `[character, list]` (mandatory, no default)
#'
#' report functions that return a report (data.frame);
#' - `character`: names of functions that can be found by `[match.fun]`
#' - `list`: list of functions
report_is_one_of <- function(x, x_nm = NULL, funs, call = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  call <- infer_call(call = call, parent.frame(1L))

  funs <- lapply(funs, match.fun)
  report_df <- do.call(rbind, lapply(funs, function(fun) {
    arg_list <- formals(fun)
    arg_list[c("x", "x_nm", "call")] <- list(x = quote(x), x_nm = quote(x_nm),
                                             call = quote(call))
    do.call(fun, arg_list)
  }))

  if (any(report_df[["pass"]])) {
    report_df <- report_df[report_df[["pass"]], ]
  } else {
    msg <- paste0(
      "    * ", report_df[["test"]], "; message: ", report_df[["message"]],
      collapse = "\n"
    )
    report_df <- data.frame(
      test = "at_least_one_of_the_following_must_pass",
      error = NA_character_,
      pass = FALSE,
      n_fail = NA_integer_,
      wh_fail = NA_integer_,
      message = paste0("\n", msg)
    )
  }

  return(report_df)
}



#' @rdname assertions
#' @export
#' @param assertion_type `[character]` (mandatory, default `"general"`)
#'
#' one of "user_input", "prod_input", etc.
assert_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL,
  assertion_type = "general"
) {

  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  call <- infer_call(call = call, parent.frame(1L))

  fun_list <- lapply(funs, match.fun)
  if (is.character(funs)) {
    names(fun_list) <- funs
  } else {
    names(fun_list) <- paste0("fun_", seq_along(fun_list))
  }
  report_df <- do.call(rbind, lapply(names(fun_list), function(fun_nm) {
    fun <- fun_list[[fun_nm]]
    arg_list <- formals(fun)
    arg_list[c("x", "x_nm", "call")] <- list(x = quote(x), x_nm = quote(x_nm),
                                             call = quote(call))
    result <- tryCatch(
      do.call(fun, arg_list),
      error = function(e) e
    )
    if (is.data.frame(result) && all(c("pass", "test") %in% names(result))) {
      return(result)
    } else if (inherits(result, c("error", "try-error"))) {
      result <- data.frame(
        test = fun_nm,
        error = result[["message"]],
        pass = FALSE,
        n_fail = NA_integer_,
        wh_fail = NA_integer_,
        message = result[["message"]]
      )
      return(result)
    } else {
      stop("Internal error: result was not data.frame nor an error. If you ",
           "can see this error, it means that the author of some function ",
           "you are using (or used by the function you are using) is ",
           "using package dbc improperly. please let them know.")
    }
  }))

  if (!any(report_df[["pass"]])) {
    msg <- paste0(
      "    * ", report_df[["test"]], "; message: ", report_df[["message"]],
      collapse = "\n"
    )
    msg <- paste0(
      "None of the following assertions passed:\n", msg
    )
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
assert_prod_input_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL
) {
  assertion_type <- "prod_input"
  assert_is_one_of(
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
  assert_is_one_of(
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
  assert_is_one_of(
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
  assert_is_one_of(
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
  assert_is_one_of(
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
  assert_is_one_of(
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
  assert_is_one_of(
    x = x,
    x_nm = x_nm,
    funs = funs,
    call = call,
    assertion_type = assertion_type
  )
}



