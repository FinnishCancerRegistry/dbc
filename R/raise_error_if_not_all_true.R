


# #' @title Yet Another Expression-to-Error Function
# #' @description
# #' Simple `stopifnot` substitute.
# #' @param ...
# #'
# #' Each expression passed to `...` is evaluated in the given order. The first
# #' to fail, if any, is raised as an error.
# #' @export
raise_error_if_not_all_true <- function(...) {
  this_call <- match.call(expand.dots = TRUE)
  ddd_expr <- match.call(expand.dots = FALSE)[["..."]]
  for (i in seq_along(ddd_expr)) {
    error_msg <- NULL
    result <- tryCatch(
      eval(ddd_expr[[i]], envir = parent.frame(1L)),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      error_msg <- paste0(
        "Evaluation of \"", deparse(ddd_expr[[i]]), "\" resulted in ",
        "this error: ", result[["message"]]
      )
    } else if (!is.vector(result) || !is.logical(result)) {
      error_msg <- paste0(
        "Evaluation of \"", deparse(ddd_expr[[i]]), "\" resulted in ",
        "this error: ", result[["message"]]
      )
    } else {
      is_fail <- !result %in% TRUE
      wh_fail <- which(is_fail)
      if (length(wh_fail) > 0L) {
        if (length(is_fail) == 1L) {
          error_msg <- paste0(
            "\"", deparse(ddd_expr[[i]]), "\" evaluated to ",
            deparse(result)
          )
        } else {
          error_msg <- paste0(
            "Evaluation results of \"", deparse(ddd_expr[[i]]), "\" were not ",
            "all TRUE. Positions of first five non-TRUE elements: ",
            deparse(utils::head(wh_fail, 5L))
          )
        }
      }
    }
    if (!is.null(error_msg)) {
      add_error_data(list(sys.calls = sys.calls(), call = this_call,
                          msg = error_msg))
      error_msg <- paste0(
        error_msg,
        ". You can use dbc::get_newest_error_data() to ",
        "see the context of the error better."
      )
      stop(simpleError(error_msg, this_call))
    }
    NULL
  }
  return(invisible(NULL))
}




