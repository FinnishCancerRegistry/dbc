




#' @rdname assertions
#' @export
#' @return
#' - `report_has_invalid_observations`: returns a report `data.frame`.
#' @param x_nm `[character]` (optional, default `NULL`)
#'
#' see e.g. [report_is_atom]
#' @param expressions `[character]` (mandatory, no default)
#'
#' passed to [expressions_to_report] as-is
#' @param fail_messages `[character]` (optional, default `NULL`)
#'
#' passed to [expressions_to_report] as-is
#' @param pass_messages `[character]` (optional, default `NULL`)
#'
#' passed to [expressions_to_report] as-is
#' @param col_nms `[character]` (optional, default `names(x)`)
#'
#' you may limit to such expressions which only use these column names; see
#' `col_nm_set_list`
#' @param col_nm_set_list `[NULL, list]` (optional, default `NULL`)
#'
#' - `NULL`: all expressions are used
#' - `list`: must be of length `length(expressions)`, where each element is a
#'   character vector of column names; the corresponding test is only run
#'   if `all(col_nm_set_list[[i]] %in% col_nms)` for all
#'   `i in seq_along(expressions)`. this allows for easy limiting of expressions to run
#'   in when not all of them need to be run.
#' @details
#' - `report_has_invalid_observations` calls [expressions_to_report] where the
#'   dataset `x` becomes the environment where the expressions are evaluated
#'   (essentially `env = as.environment(x)`)
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   expressions = c("a == b", "a == d", "a != b")
#' )
#'
#' # running expressions only for some columns
#' df <- data.frame(a = 1:5, b = 1:5, c = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   expressions = c("a == b", "a == c", "a != b"),
#'   col_nms = c("a", "b"),
#'   col_nm_set_list = list(c("a", "b"), c("a", "c"), c("a", "b"))
#' )
report_has_only_valid_observations <- function(
  x,
  x_nm = NULL,
  expressions,
  fail_messages = NULL,
  pass_messages = NULL,
  col_nms = names(x),
  col_nm_set_list = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  assert_is_data.frame(x = x, x_nm = x_nm)
  assert_is_character_nonNA_vector(expressions)
  assert_is_one_of(
    x = col_nm_set_list,
    funs = c("report_is_NULL", "report_is_list")
  )
  parent_env <- parent.frame(1L)
  dataset_env <- as.environment(x)
  parent.env(dataset_env) <- parent_env
  call <- dbc::handle_arg_call(call)

  which_tests_to_run <- seq_along(expressions)
  if (inherits(col_nm_set_list, "list")) {
    lapply(col_nm_set_list, assert_is_character_nonNA_vector)
    assert_is_character_nonNA_vector(col_nms)
    assert_vector_elems_are_in_set(col_nms, set = names(x))
    assert_is_of_length(col_nm_set_list, expected_length = length(expressions))

    which_tests_to_run <- which(vapply(
      seq_along(expressions),
      function(test_no) {
        test_col_nm_set <- col_nm_set_list[[test_no]]
        all(test_col_nm_set %in% col_nms)
      },
      logical(1L)
    ))
    if (length(which_tests_to_run) == 0L) {
      report_df <- expressions_to_report(
        expressions = "1 == 1",
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

  expressions_to_report(
    expressions = expressions[which_tests_to_run],
    fail_messages = fail_messages,
    pass_messages = pass_messages,
    env = dataset_env
  )
}

get_report_df_template <- function() {
  report_df_template #internal dataset
}
#' @title `report_df` Report
#' @description
#' Report whether an object is a valid report data.frame as produced by e.g.
#' `[dbc::expressions_to_report]`.
#' @template arg_x
#' @template arg_x_nm
#' @template arg_call
#' @export
report_is_report_df <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  expected_col_nms <- names(get_report_df_template())
  miss_col_nms <- setdiff(names(expected_col_nms), names(x))

  expressions <- c(
    "is.data.frame(x)",
    paste0("all(", deparse1(expected_col_nms), " %in% names(x))")
  )
  fail_msgs <- c(
    paste0(deparse1(x_nm), " was not a data.frame"),
    paste0(deparse1(x_nm), " did not contain all the expected columns; it ",
           "had columns ", deparse1(names(x)), "; expected ",
           deparse1(expected_col_nms), "; missing columns ",
           deparse1(miss_col_nms))
  )
  fail_msgs <- paste0(
    fail_msgs, "; if you can see this error, the command you used has one or ",
    "more misspecified assertions, and you should complain to the command's ",
    "maintainer."
  )
  report_df <- dbc::expressions_to_report(
    expressions,
    fail_messages = fail_msgs,
    call = call
  )
  return(report_df)
}







