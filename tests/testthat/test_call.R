testthat::test_that("assert_* funs show correct call in error msg", {
  tf <- function(arg) {
    dbc::assert_user_input_is_character_nonNA_atom(arg)
  }
  err <- tryCatch(tf(1L), error = function(e) e)
  testthat::expect_true(
    grepl("\\Qtf(arg = 1L)\\E", err$message)
  )
  testthat::expect_true(
    grepl("\\Qtf(arg = 1L)\\E", deparse(err$call))
  )

})

testthat::test_that("report_to_assertion shows correct call in error msg", {
  tf <- function(arg) {
    dbc::report_to_assertion(
      dbc::expressions_to_report(
        expressions = "is.character(arg)"
      )
    )

  }
  err <- tryCatch(tf(1L), error = function(e) e)
  testthat::expect_true(grepl("\\Qtf(arg = 1L)\\E", err$message))
  testthat::expect_true(grepl("\\Qtf(arg = 1L)\\E", deparse(err$call)))
})
