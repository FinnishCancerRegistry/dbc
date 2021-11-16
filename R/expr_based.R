

#' @rdname assertions
#' @export
report_is <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_expr <- substitute(x)
  tests_to_report(
    tests = list(x_expr)
  )
}

generate_assert_is_funs <- function() {

  lines <- unlist(lapply(assertion_types(), function(assertion_type) {
    if (assertion_type == "general") {
      fun_nm <- "assert_is"
    } else {
      fun_nm <- paste0("assert_", assertion_type, "_is")
    }

    lines <- c(
      "",
      "# this function was generated automatically. do not edit by hand!",
      "#' @rdname assertions",
      "#' @export",
      "%%FUN_NM%% <- function(",
      "  x,",
      "  x_nm = NULL,",
      "  call = NULL",
      ") {",
      "  x_nm <- handle_x_nm_arg(x_nm)",
      "  call <- handle_arg_call(call = call, env = parent.frame(1L))",
      "  if (is.null(call)) {",
      "    call <- match.call()",
      "  }",
      "  report_df <- eval(substitute(",
      "    report_is(x = x, x_nm = x_nm, call = call),",
      "    list(x = substitute(x))",
      "  ))",
      "  report_df[[\"call\"]][[1L]] <- call",
      "  report_to_assertion(",
      "    report_df,",
      "    assertion_type = \"%%ASSERTION_TYPE%%\",",
      "    raise_error_call = call",
      "  )",
      "}",
      ""
    )
    lines <- gsub(
      "%%FUN_NM%%", fun_nm, lines
    )
    lines <- gsub(
      "%%ASSERTION_TYPE%%", assertion_type, lines
    )
  }))

  lines <- c(
    "# this script was generated automatically. do not edit by hand!",
    "",
    "",
    lines
  )

  writeLines(lines, "R/generated_assert_is_funs.R")
}









