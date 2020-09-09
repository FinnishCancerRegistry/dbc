


#' @rdname dataset_validation
#' @export
#' @param report_df `[data.frame]` (mandatory, no default)
#'
#' a report `data.frame` as produced by e.g. [tests_to_report]
#' @return
#' - `identify_invalid_observations`: returns a data.frame with columns
#'   `is_valid`, `fail_test_set`, and `fail_message_set`; new columns are
#'   populated based on `report_df` contents; the data.frame has `nrow(x)` rows
#' @examples
#'
#' df <- data.frame(a = 1:5, b = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   tests = c("a == b", "a == d", "a != b")
#' )
#'
#' identify_invalid_observations(x = df, report_df = report_df)
identify_invalid_observations <- function(
  x,
  report_df
) {
  assert_is_data.frame(x)
  assert_is_data.frame_with_required_names(
    report_df,
    required_names = c("pass", "wh_fail", "test", "message")
  )
  result_df <- data.frame(is_valid = rep(TRUE, nrow(x)))
  result_df[["fail_test_set"]] <- NA_character_
  result_df[["fail_message_set"]] <- NA_character_
  .__fun_env <- environment()
  fail_test_no_set <- which(!report_df[["pass"]])
  invisible(lapply(fail_test_no_set, function(test_no) {
    wh_fail <- report_df[["wh_fail"]][[test_no]]
    is_error <- !is.na(report_df[["error"]][test_no])
    if (is_error) {
      wh_fail <- 1:nrow(result_df)
    }
    .__fun_env$result_df[["is_valid"]][wh_fail] <- FALSE

    fail_test <- report_df[["test"]][test_no]
    fail_test_set <- result_df[["fail_test_set"]][wh_fail]
    fail_test_set <- ifelse(
      is.na(fail_test_set),
      fail_test,
      paste0(fail_test_set, "; ", fail_test)
    )
    .__fun_env$result_df[["fail_test_set"]][wh_fail] <- fail_test_set

    fail_message <- report_df[["message"]][test_no]
    if (is_error) {
      fail_message <- report_df[["error"]][test_no]
    }
    fail_message_set <- result_df[["fail_message_set"]][wh_fail]
    fail_message_set <- ifelse(
      is.na(fail_message_set),
      fail_message,
      paste0(fail_message_set, "; ", fail_message)
    )
    .__fun_env$result_df[["fail_message_set"]][wh_fail] <- fail_message_set
    NULL
  }))

  return(result_df[])
}






