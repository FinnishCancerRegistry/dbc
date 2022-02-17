testthat::context("assertion types")

testthat::test_that("all metadata exists for each assertion type", {

  at   <- dbc::assertion_types()
  ats  <- dbc:::assertion_type_summaries()
  atem <- dbc:::assertion_type_error_messages()
  testthat::expect_identical(at, names(ats))
  testthat::expect_identical(at, names(atem))

})
