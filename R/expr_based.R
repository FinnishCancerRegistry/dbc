

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








