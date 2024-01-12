out <- out && local({
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
  if (inherits(test_result, "error")) {
    stop(simpleError(
      paste0(
        "Test `EXPRESSION` resulted in error: ", test_result[["message"]]
      ),
      call = call
    ))
    test_result <- FALSE
  } else if (is.null(test_result)) {
    test_result <- TRUE
  } else if (!is.logical(test_result)) {
    stop(simpleError(
      paste0(
        "Internal error: Test `EXPRESSION` is misspecified. ",
        "It resulted in output of class(es) ",
        deparse1(test_result),
        ". Output should be either logical or NULL."
      ),
      call = call
    ))
  }
  all(test_result)
})