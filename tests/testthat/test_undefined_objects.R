




testthat::test_that("funs handle missing objects gracefully", {

  result <- tryCatch(
    dbc::assert_is_atom(NONEXISTENT_OBJECT),
    warning = function(w) w,
    error = function(e) e
  )
  testthat::expect_s3_class(result, "error")

})






