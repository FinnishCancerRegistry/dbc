
get_function <- function(x, env = environment(get_function)) {
  if (is.character(x)) {
    fun <- tryCatch(
      get(x = x, envir = env, mode = "function"),
      error = function(e) e
    )
    if (inherits(fun, "error")) {
      fun <- tryCatch(
        eval(parse(text = x)[[1]], envir = env),
        error = function(e) e
      )
    }
    if (inherits(fun, "error")) {
      stop("Internal error: Cannot find function based on string ", deparse(x),
           "; if you can see this, complain to the maintainer of the command ",
           "you just used")
    }
  } else if (!is.function(x)) {
    stop("Internal error: neither string nor function passed to get_function; ",
         "if you can see this, complain to the maintainer of the command ",
         "you just used")
  } else {
    fun <- x
  }
  return(fun)
}

#' @rdname assertions
#' @export
#' @param funs `[character, list]` (mandatory, no default)
#'
#' report functions that return a report (data.frame);
#' - `character`: names of functions that can be found by `[match.fun]`
#' - `list`: list of functions
#' @examples
#' my_var <- 1:3
#' df1 <- dbc::report_is_one_of(
#'   my_var,
#'   funs = list(dbc::report_is_NULL, dbc::report_is_character_nonNA_vector)
#' )
#' df2 <- dbc::report_is_NULL(my_var)
#' df3 <- dbc::report_is_character_nonNA_vector(my_var)
#' stopifnot(
#'   identical(names(df1), names(df2)),
#'   identical(lapply(df1, class), lapply(df2, class)),
#'   nrow(df1) == 2L,
#'   nrow(df2) + nrow(df3) > 2L,
#'   is.na(df1[["error"]])
#' )
#'
report_is_one_of <- function(x, x_nm = NULL, call = NULL, funs) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_is_one_of_call <- match.call()
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  funs <- as.list(funs)
  funs <- lapply(seq_along(funs), function(i) {
    fun <- funs[[i]]
    fun <- get_function(fun)
    dbc::assert_prod_input_is_function_with_required_argument_names(
      fun,
      x_nm = paste0("funs[[", i, "]]"),
      call = report_is_one_of_call,
      required_argument_names = c("x", "x_nm", "call")
    )
    fun
  })
  report_df <- do.call(rbind, lapply(seq_along(funs), function(i) {
    fun <- funs[[i]]
    report_df <- tryCatch(fun(x = x, x_nm = x_nm, call = call),
                          error = function(e) e)
    dbc::assert_prod_interim_is_report_df(
      report_df,
      x_nm = paste0("funs[[", i,"]](x = x, x_nm = x_nm, call = call)"),
      call = report_is_one_of_call
    )
    report_df[["all_pass"]] <- all(report_df[["pass"]] %in% TRUE)
    total_report_df <- data.frame(
      test = paste0(report_df[["test"]], collapse = " & "),
      error = NA_character_,
      pass = all(report_df[["pass"]] %in% TRUE),
      n_fail = NA_integer_,
      wh_fail = NA_character_,
      message = paste0(report_df[["message"]], collapse = "; "),
      call = NA_character_
    )

    errors <- report_df[["error"]]
    if (!all(is.na(errors))) {
      errors[is.na(errors)] <- "no error"
      total_report_df[["error"]] <- paste0(errors, collapse = "; ")
    }
    total_report_df[["wh_fail"]] <- list(list_union(
      as.list(report_df[["wh_fail"]])
    ))
    total_report_df[["n_fail"]] <- length(total_report_df[["wh_fail"]])
    total_report_df[["call"]] <- list(report_df[["call"]][[1L]])
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
  call = NULL,
  funs,
  assertion_type = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  call <- dbc::handle_arg_call(call)
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)

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
      stop(msg, call. = call)
    }
  }
  invisible(NULL)
  return(invisible(NULL))
}

#' @rdname assertions
#' @export
assert_is_one_of <- function(
  x,
  x_nm = NULL,
  funs,
  call = NULL,
  assertion_type = NULL
) {
  assert_is_one_of__(
    x = x, x_nm = x_nm, funs = funs, call = call,
    assertion_type = assertion_type
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



