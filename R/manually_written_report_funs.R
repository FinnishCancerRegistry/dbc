




#' @rdname assertions
#' @export
#' @return
#' - `report_has_invalid_observations`: returns a report `data.frame`.
#' @param x_nm `[character]` (optional, default `NULL`)
#'
#' see e.g. [report_is_atom]
#' @param tests `[character]` (mandatory, no default)
#'
#' passed to [tests_to_report] as-is
#' @param fail_messages `[character]` (optional, default `NULL`)
#'
#' passed to [tests_to_report] as-is
#' @param pass_messages `[character]` (optional, default `NULL`)
#'
#' passed to [tests_to_report] as-is
#' @param col_nms `[character]` (optional, default `names(x)`)
#'
#' you may limit to such tests which only use these column names; see
#' `col_nm_set_list`
#' @param col_nm_set_list `[NULL, list]` (optional, default `NULL`)
#'
#' - `NULL`: all tests are used
#' - `list`: must be of length `length(tests)`, where each element is a
#'   character vector of column names; the corresponding test is only run
#'   if `all(col_nm_set_list[[i]] %in% col_nms)` for all
#'   `i in seq_along(tests)`. this allows for easy limiting of tests to run
#'   in when not all of them need to be run.
#' @details
#' - `report_has_invalid_observations` calls [tests_to_report] where the
#'   dataset `x` becomes the environment where the tests are evaluated
#'   (essentially `env = as.environment(x)`)
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   tests = c("a == b", "a == d", "a != b")
#' )
#'
#' # running tests only for some columns
#' df <- data.frame(a = 1:5, b = 1:5, c = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   tests = c("a == b", "a == c", "a != b"),
#'   col_nms = c("a", "b"),
#'   col_nm_set_list = list(c("a", "b"), c("a", "c"), c("a", "b"))
#' )
report_has_only_valid_observations <- function(
  x,
  x_nm = NULL,
  tests,
  fail_messages = NULL,
  pass_messages = NULL,
  col_nms = names(x),
  col_nm_set_list = NULL,
  call = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_data.frame(x = x, x_nm = x_nm)
  assert_is_character_nonNA_vector(tests)
  assert_is_one_of(
    x = col_nm_set_list,
    funs = c("report_is_NULL", "report_is_list")
  )
  parent_env <- parent.frame(1L)
  dataset_env <- as.environment(x)
  parent.env(dataset_env) <- parent_env
  call <- infer_call(call = call, parent.frame(1L))
  if (is.null(call)) {
    call <- match.call()
  }

  which_tests_to_run <- seq_along(tests)
  if (inherits(col_nm_set_list, "list")) {
    lapply(col_nm_set_list, assert_is_character_nonNA_vector)
    assert_is_character_nonNA_vector(col_nms)
    assert_vector_elems_are_in_set(col_nms, set = names(x))
    assert_is_of_length(col_nm_set_list, expected_length = length(tests))

    which_tests_to_run <- which(vapply(
      seq_along(tests),
      function(test_no) {
        test_col_nm_set <- col_nm_set_list[[test_no]]
        all(test_col_nm_set %in% col_nms)
      },
      logical(1L)
    ))
    if (length(which_tests_to_run) == 0L) {
      report_df <- tests_to_report(
        tests = "1 == 1",
        fail_messages = fail_messages,
        pass_messages = pass_messages,
        env = dataset_env
      )
      return(report_df[0L, ])
    }
  }

  if (length(fail_messages) == 1L) {
    fail_messages <- rep(fail_messages, length(which_tests_to_run))
  }
  if (length(pass_messages) == 1L) {
    pass_messages <- rep(pass_messages, length(which_tests_to_run))
  }

  tests_to_report(
    tests = tests[which_tests_to_run],
    fail_messages = fail_messages,
    pass_messages = pass_messages,
    env = dataset_env
  )
}








