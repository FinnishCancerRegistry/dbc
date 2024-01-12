local({
  eval_env <- new.env(parent = environment())
  test_result <- local(
    {
      tryCatch(
        EXPRESSION,
        error = function(e) e
      )
    },
    envir = eval_env
  )
  fail_message <- FAIL_MESSAGE
  if (inherits(test_result, "error")) {
    fail_message <- paste0(
      "Test `EXPRESSION` resulted in error: ",
      test_result[["message"]]
    )
    test_result <- FALSE
  } else if (is.null(test_result)) {
    test_result <- TRUE
  } else if (!is.logical(test_result)) {
    stop(
      "Internal error: Test `EXPRESSION` is misspecified. ",
      "It resulted in output of class(es) ",
      deparse1(test_result),
      ". Output should be either logical or NULL."
    )
  }
  if (!all(test_result %in% TRUE)) {
    eval_env$wh_fail <- which(!test_result)
    eval_env$n_fail <- length(eval_env$wh_fail)
    fail_message <- interpolate(fail_message, env = eval_env)
    stop(simpleError(message = fail_message, call = call))
  }
})