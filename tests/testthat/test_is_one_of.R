




testthat::test_that("dbc::report_is_one_of works", {


  report_df <- dbc::report_is_one_of(
    x = 1L,
    funs = c("report_is_data.table", "report_is_integer")
  )

  testthat::expect_identical(report_df[["pass"]], c(FALSE, TRUE))

  report_df <- dbc::report_is_one_of(
    x = 1L,
    funs = list(dbc::report_is_data.table, dbc::report_is_integer)
  )

  testthat::expect_identical(report_df[["pass"]], c(FALSE, TRUE))

})



testthat::test_that("dbc::assert_is_one_of works", {

  tf <- function(x) {
    dbc::assert_is_one_of(
      x = 1L,
      funs = c("report_is_data.table", "report_is_integer")
    )
  }

  tf(1L) # no error

  tf <- function(x) {
    dbc::assert_is_one_of(
      x = "wat",
      funs = c("report_is_data.table", "report_is_integer")
    )
  }
  # should cause error
  testthat::expect_error(
    tf(1L),
    regexp = "\\QNone of the following assertions passed:\\E"
  )

  tf <- function(x) {
    dbc::assert_is_one_of(
      x = "wat",
      funs = c("report_is_data.table", "report_is_integer")
    )
  }
  # should cause error
  testthat::expect_error(
    tf(1L),
    regexp = "\\QNone of the following assertions passed:\\E"
  )

  tf <- function(x) {
    dbc::assert_is_one_of(
      x = "wat",
      funs = list(dbc::report_is_data.table, dbc::report_is_integer)
    )
  }
  # should cause error
  testthat::expect_error(
    tf(1L),
    regexp = "\\QNone of the following assertions passed:\\E"
  )

  bad_report_fun_1 <- function(y) {
    dbc::report_is_NULL(x = y)
  }
  tf <- function(x) {
    dbc::assert_is_one_of(
      x = x,
      funs = list(dbc::report_is_data.table, bad_report_fun_1)
    )
    x
  }
  testthat::expect_error(
    tf(1L),
    regexp = "\\Qdid not have all required arguments\\E"
  )

  bad_report_fun_2 <- function(x, x_nm, call) {
    data.frame(pass = TRUE)
  }
  tf <- function(x) {
    dbc::assert_is_one_of(
      x = x,
      funs = list(dbc::report_is_data.table, bad_report_fun_2)
    )
    x
  }
  testthat::expect_error(
    tf(1L),
    regexp = "\\QFollowing columns were expected but not in\\E"
  )

  bad_report_fun_3 <- function(x, x_nm, call) {
    NULL
  }
  tf <- function(x) {
    dbc::assert_is_one_of(
      x = x,
      funs = list(dbc::report_is_data.table, bad_report_fun_3)
    )
    x
  }
  testthat::expect_error(
    tf(1L),
    regexp = "\\Qwas not a data.frame\\E"
  )
})

