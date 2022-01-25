
testthat::test_that("interpolate works as intended", {

  tf <- function(x) {
    dbc::assert_user_input_is_integer(x)
    return(x + 1L)
  }
  tryCatch(
    tf("wat"),
    error = function(e) NULL
  )
  ed <- dbc::get_newest_error_data()
  testthat::expect_named(ed, expected = c("call", "msg", "sys.calls"))
  testthat::expect_equal(
    ed[["sys.calls"]][[length(ed[["sys.calls"]]) - 1L]],
    quote(tf("wat"))
  )

})

