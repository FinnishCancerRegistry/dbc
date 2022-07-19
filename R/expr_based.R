

#' @rdname assertions
#' @export
#' @param env `[NULL, environment]` (default `NULL`)
#'
#' - `NULL`: Take the env where this function was called.
#' - `environment`: Use this environment.
#'
#' Passed to `[dbc::expressions_to_report]`.
report_is <- function(
  x,
  x_nm = NULL,
  call = NULL,
  env = NULL
) {
  # @codedoc_comment_block news("dbc::report_is", "2022-07-19", "0.4.6")
  # `dbc::report_is` gains arg `env` it is passed to
  # `dbc::expressions_to_report`, so that's where `x` will be evaluated.
  # @codedoc_comment_block news("dbc::report_is", "2022-07-19", "0.4.6")
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  x_expr <- substitute(x)
  dbc::expressions_to_report(
    expressions = list(x_expr),
    env = env
  )
}

generate_assert_is_funs <- function() {

  lines <- unlist(lapply(dbc::assertion_types(), function(assertion_type) {
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
      "  x_nm <- dbc::handle_arg_x_nm(x_nm)",
      "  call <- dbc::handle_arg_call(call)",
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









