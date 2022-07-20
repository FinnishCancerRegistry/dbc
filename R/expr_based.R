

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
#' df_1 <- dbc::report_is(1 + 1 == 2)
#' df_2 <- dbc::report_is("1 + 1 == 2")
#' df_3 <- dbc::report_is(list(quote(1 + 1 == 2), "1 + 1 == 2"))
#' stopifnot(
#'   df_1[["pass"]],
#'   df_2[["pass"]],
#'   df_3[["pass"]]
#' )
#'
#' # dbc::assert_is
#' dbc::assert_is(1 + 1 == 2)
#' dbc::assert_is("1 + 1 == 2")
#' dbc::assert_is(list(quote(1 + 1 == 2), "1 + 1 == 2"))
#' my_fun <- function(x) {
#'   dbc::assert_is(length(x) == 1)
#'   dbc::assert_is("length(x) == 1")
#'   dbc::assert_is(list(quote(length(x) == 1), "length(x) == 1"))
#'   x ^ 2
#' }
#' my_fun(1L)
#' my_assert_is_fun <- function(x) {
#'   env <- parent.frame(1L)
#'   dbc::assert_is(x, env = env)
#' }
#' my_obj <- 1:3
#' my_assert_is_fun("length(my_obj) == 3")
#'
report_is <- function(
  x,
  x_nm = NULL,
  call = NULL,
  env = NULL
) {
  # @codedoc_comment_block news("dbc::report_is", "2022-07-20", "0.4.7")
  # `dbc::report_is` and all corresponding assertion functions now handle
  # string and expression inputs more robustly.
  # @codedoc_comment_block news("dbc::report_is", "2022-07-20", "0.4.7")

  # @codedoc_comment_block news("dbc::report_is", "2022-07-19", "0.4.6")
  # `dbc::report_is` gains arg `env` it is passed to
  # `dbc::expressions_to_report`, so that's where `x` will be evaluated.
  # @codedoc_comment_block news("dbc::report_is", "2022-07-19", "0.4.6")
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  x_expr <- tryCatch(
    is.character(x) || is.language(x),
    error = function(e) e,
    warning = function(w) w
  )
  if (!identical(x_expr, TRUE)) {
    x_expr <- substitute(x)
  } else {
    x_expr <- x
  }
  if (is.call(x_expr) && identical(x_expr[[1]], quote(list))) {
    # assume a list of quoted / substituted expressions or strings
    expressions <- eval(x_expr, envir = env)
  } else {
    expressions <- list(x_expr)
  }
  dbc::expressions_to_report(
    expressions = expressions,
    env = env
  )
}








