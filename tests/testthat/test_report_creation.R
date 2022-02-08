


testthat::test_that("can create reports from both strings & lang objs", {

  my_obj <- 1:5
  my_tests <- list(
    "my_obj > 0L",
    quote(my_obj > 0L)
  )

  df <- expressions_to_report(
    expressions = my_tests
  )

  # as.list only to get rid of row names
  testthat::expect_equal(as.list(df[1L, ]), as.list(df[2L, ]))
})

