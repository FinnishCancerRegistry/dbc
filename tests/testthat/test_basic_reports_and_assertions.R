



testthat::test_that("basic report / assertion funs work", {

  df_pass <- report_is_integer_nonNA_gtzero_atom(1L)
  df_fail <- report_is_integer_nonNA_gtzero_atom(0L)

  testthat::expect_true(all(df_pass[["pass"]]))
  testthat::expect_true(!all(df_fail[["pass"]]))

  assert_is_integer_nonNA_gtzero_atom(1L)
  testthat::expect_error(
    assert_is_integer_nonNA_gtzero_atom(0L)
  )

  dbc::assert_atom_is_in_set(x = 0L, set = 0:5)
  testthat::expect_error(
    dbc::assert_atom_is_in_set(x = 0L, set = 1:5)
  )

})


testthat::test_that("enclosing function call included in report", {

  my_fun <- function(x) {
    dbc::assert_is_integer_gtzero_atom(x)
    x ^ 5
  }

  call_string <- "my_fun(x = 0L)"
  call <- parse(text = call_string)[[1L]]

  err <- tryCatch(eval(call), error = function(e) e)
  testthat::expect_identical(
    deparse1(err[["call"]]),
    call_string
  )
})


testthat::test_that("n_fail used even for atomic input", {
  report_df <- dbc::report_is_nonNA(x = NA)
  testthat::expect_equal(
    report_df[["n_fail"]][1L],
    1L
  )
  report_df <- dbc::report_is_nonNA(x = 1)
  testthat::expect_equal(
    report_df[["n_fail"]][1L],
    0L
  )
})




