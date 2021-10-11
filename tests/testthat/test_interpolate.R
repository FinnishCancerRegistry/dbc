

testthat::test_that("interpolate works as intended", {

  x <- 1:5
  set <- 1:2
  x_nm <- "x"
  result <- interpolate(x = "bad ${x_nm} values: ${deparse(setdiff(x, set))}")
  testthat::expect_identical(
    result,
    "bad x values: 3:5"
  )

  rm("set")
  result <- interpolate(x = "bad ${x_nm} values: ${deparse(setdiff(x, set))}")
  testthat::expect_identical(
    result,
    "bad x values: deparse(setdiff(x, set))"
  )

})


