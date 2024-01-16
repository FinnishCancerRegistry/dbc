

#' @rdname assertions
#' @export
#' @param env `[NULL, environment]` (default `NULL`)
#'
#' - `NULL`: Take the env where this function was called.
#' - `environment`: Use this environment.
#'
#' Passed to `[dbc::expressions_to_report]`.
#' @examples
#'
#' # dbc::report_is
#' df_1 <- dbc::report_is(quote(1 + 1 == 2))
#' df_2 <- dbc::report_is(list(quote(1 + 1 == 2), quote(2 + 2 == 4)))
#' stopifnot(
#'   df_1[["pass"]],
#'   df_2[["pass"]]
#' )
#'
#' # dbc::assert_is
#' dbc::assert_is(quote(1 + 1 == 2))
#' dbc::assert_is(list(quote(1 + 1 == 2), quote(2 + 2 == 4)))
#' my_fun <- function(x) {
#'   dbc::assert_is(quote(length(x) == 1))
#'   dbc::assert_is(list(quote(length(x) == 1), quote(is.numeric(x))))
#'   x ^ 2
#' }
#' my_fun(1L)
report_is <- function(
  x,
  x_nm = NULL,
  call = NULL,
  env = NULL
) {
  # @codedoc_comment_block news("dbc::report_is", "2024-01-16", "0.5.0")
  # `dbc::report_is` now only accepts objects of type `call` and `list`.
  # A `list` is assumed to contain `call`s.
  # @codedoc_comment_block news("dbc::report_is", "2024-01-16", "0.5.0")

  # @codedoc_comment_block news("dbc::report_is", "2022-07-20", "0.4.7")
  # `dbc::report_is` and all corresponding assertion functions now handle
  # string and expression inputs more robustly.
  # @codedoc_comment_block news("dbc::report_is", "2022-07-20", "0.4.7")

  # @codedoc_comment_block news("dbc::report_is", "2022-07-19", "0.4.6")
  # `dbc::report_is` gains arg `env` it is passed to
  # `dbc::expressions_to_report`, so that's where `x` will be evaluated.
  # @codedoc_comment_block news("dbc::report_is", "2022-07-19", "0.4.6")
  # dbc::assert_prod_input_is_one_of(
  #   x = x,
  #   funs = list(dbc::report_is_language_object,
  #               dbc::report_is_list)
  # )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  if (!is.list(x)) {
    x <- list(x)
  }
  dbc::expressions_to_report(
    expressions = x,
    env = env
  )
}

#' @rdname assertions
#' @export
#' @examples
#'
#' # dbc::assert
#' my_var <- 1
#' dbc::assert(length(my_var) == 1, my_var > 0, is.numeric(my_var))
#' my_fun <- function(x) {
#'   dbc::assert(
#'     length(x) == 1,
#'     x > 0,
#'     is.numeric(x)
#'   )
#'   x ^ 2
#' }
#' my_fun(1L)
assert <- function(
  ...,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("dbc::assert", "2024-01-16", "0.5.0")
  # New exported function `dbc::assert`. Evaluate arbitrary assertion
  # expressions.
  # @codedoc_comment_block news("dbc::assert", "2024-01-16", "0.5.0")
  dbc::handle_args_inplace()
  expr_set <- as.list(substitute(list(...))[-1])
  env <- parent.frame(1L)
  for (expr in expr_set) {
    dbc::assertion_eval(
      expression = expr,
      fail_message = NA_character_,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      env = env
    )
  }
}


#' @rdname assertions
#' @export
#' @examples
#'
#' # dbc::report
#' my_var <- 1
#' df <- dbc::report(length(my_var) == 1, my_var > 0, is.numeric(my_var))
#' stopifnot(
#'   is.data.frame(df)
#' )
report <- function(
  ...,
  x_nm = NULL,
  call = NULL
) {
  # @codedoc_comment_block news("dbc::report", "2024-01-16", "0.5.0")
  # New exported function `dbc::report`. Evaluate arbitrary report
  # expressions.
  # @codedoc_comment_block news("dbc::report", "2024-01-16", "0.5.0")
  dbc::handle_args_inplace()
  expr_set <- as.list(substitute(list(...))[-1])
  env <- parent.frame(1L)
  report_is(
    x = expr_set,
    x_nm = x_nm,
    call = call,
    env = env
  )
}


#' @rdname assertions
#' @export
#' @examples
#'
#' # dbc::test
#' stopifnot(
#'   dbc::test(length(my_var) == 1, my_var > 0, is.numeric(my_var))
#' )
test <- function(
  ...,
  x_nm = NULL,
  call = NULL
) {
  # @codedoc_comment_block news("dbc::test", "2024-01-16", "0.5.0")
  # New exported function `dbc::test`. Evaluate arbitrary test
  # expressions.
  # @codedoc_comment_block news("dbc::test", "2024-01-16", "0.5.0")
  dbc::handle_args_inplace()
  expr_set <- as.list(substitute(list(...))[-1])
  env <- parent.frame(1L)
  out <- TRUE
  for (expr in expr_set) {
    result <- dbc::expression_eval(expression = expr, eval_parent_env = env)
    if (!is.na(result[["error"]])) {
      stop(simpleError(
        message = result[["error"]],
        call = call
      ))
    }
    out <- out && identical(result[["pass"]], TRUE)
    if (!out) {
      break
    }
  }
  return(out)
}

