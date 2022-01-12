




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
})
