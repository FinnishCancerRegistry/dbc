







get_report_fun_specs <- function() {
  report_fun_specs
}


get_report_fun_df <- function() {
  requireNamespace("data.table")
  specs_df <- get_report_fun_specs()
  specs_df_col_nms <- c("test_set_nm", "call",
                        "fail_message", "pass_message",
                        "extra_arg_nm_set")
  raise_internal_error_if_not(
    is.data.frame(specs_df),
    specs_df_col_nms %in% names(specs_df)
  )
  specs_df[, names(specs_df)] <- lapply(specs_df, as.character)
  base_prefix <- "report_"
  fun_df <- data.table::copy(specs_df)

  deriv_fun_df <- report_function_variant_space()
  deriv_fun_df <- data.table::melt(
    deriv_fun_df,
    id.vars = "fun_nm",
    measure.vars = setdiff(names(deriv_fun_df), "fun_nm"),
    value.name = "test_set_nm",
    variable.name = "type"
  )
  data.table::setnames(deriv_fun_df, "fun_nm", "deriv_fun_nm")
  data.table::set(deriv_fun_df, j = "type", value = NULL)
  data.table::set(deriv_fun_df, j = "test_set_nm", value = sub(
    "^report_",
    "",
    sub("\\(.+", "", deriv_fun_df[["test_set_nm"]])
  ))
  deriv_fun_df <- merge(
    x = fun_df,
    y = deriv_fun_df,
    by = "test_set_nm",
    all.x = FALSE, all.y = FALSE
  )
  data.table::setDT(deriv_fun_df)
  data.table::set(deriv_fun_df, j = "test_set_nm", value = sub(
    "^report_", "", deriv_fun_df[["deriv_fun_nm"]]
  ))
  data.table::set(deriv_fun_df, j = "deriv_fun_nm", value = NULL)
  data.table::setcolorder(deriv_fun_df, names(fun_df))

  fun_df <- rbind(fun_df, deriv_fun_df)
  rm("deriv_fun_df")
  data.table::setDF(fun_df)
  fun_df[["fun_nm"]] <- paste0(base_prefix, fun_df[["test_set_nm"]])
  fun_df <- unique(fun_df, by = c("test_set_nm", "call"))

  fun_df <- stats::aggregate(
    fun_df[, c("call", "fail_message", "pass_message", "extra_arg_nm_set")],
    by = list(fun_nm = fun_df[["fun_nm"]], test_set_nm = fun_df[["test_set_nm"]]),
    FUN = function(x) list(unlist(x))
  )
  data.table::setnames(
    fun_df,
    c("call", "fail_message", "pass_message"),
    c("expression_set", "fail_message_set", "pass_message_set")
  )
  return(fun_df)
}



generate_report_funs <- function(
  target_script = "R/generated_report_funs.R"
) {
  requireNamespace("data.table")
  fun_df <- get_report_fun_df()
  fun_df_col_nms <- c(
    "test_set_nm", "fun_nm",
    "expression_set", "fail_message_set", "pass_message_set", "extra_arg_nm_set"
  )
  stopifnot(
    is.character(target_script),
    length(target_script) == 1L,
    !is.na(target_script),

    is.data.frame(fun_df),
    fun_df_col_nms %in% names(fun_df),
    !duplicated(fun_df[["fun_nm"]])
  )


  fun_df[["body"]] <- lapply(seq_len(nrow(fun_df)), function(fun_no) {
    expression_set <- fun_df[["expression_set"]][[fun_no]]
    fail_message_set <- fun_df[["fail_message_set"]][[fun_no]]
    pass_message_set <- fun_df[["pass_message_set"]][[fun_no]]
    expression_set <- paste0("\"", expression_set, "\"")
    fail_message_set <- paste0("\"", fail_message_set, "\"")
    pass_message_set <- paste0("\"", pass_message_set, "\"")
    n <- length(expression_set)
    if (n > 1L) {
      expression_set[1:(n - 1L)] <- paste0(expression_set[1:(n - 1L)], ", ")
      fail_message_set[1:(n - 1L)] <- paste0(fail_message_set[1:(n - 1L)], ", ")
      pass_message_set[1:(n - 1L)] <- paste0(pass_message_set[1:(n - 1L)], ", ")
    }

    body <- paste0("  ", c(
      generated_function_header(),
      "report_env <- environment()",
      "expression_set <- c(",
      paste0("  ", expression_set),
      ")",
      "fail_message_set <- c(",
      paste0("  ", fail_message_set),
      ")",
      "pass_message_set <- c(",
      paste0("  ", pass_message_set),
      ")",
      "report_df <- dbc::expressions_to_report(",
      "  expressions = expression_set,",
      "  fail_messages = fail_message_set,",
      "  pass_messages = pass_message_set,",
      "  env = report_env, ",
      "  call = call",
      ")",
      "return(report_df)"
    ))
    body
  })
  fun_df[["fun_def"]] <- lapply(seq_len(nrow(fun_df)), function(fun_no) {
    body <- fun_df[["body"]][[fun_no]]
    arg_set <- c("x", "x_nm = NULL", "call = NULL")
    arg_set <- setdiff(
      c(arg_set, fun_df[["extra_arg_nm_set"]][[fun_no]]),
      c(NA_character_, "")
    )
    arg_set <- paste0(arg_set, collapse = ", ")
    def <- c(
      "#' @rdname assertions",
      "#' @export",
      paste0(fun_df[["fun_nm"]][fun_no], " <- function(", arg_set, ") {"),
      body,
      "}"
    )
    return(def)
  })

  lines <- c(
    "# this script was generated automatically. do not edit by hand!",
    rep("", 5),
    unlist(lapply(fun_df[["fun_def"]], function(x) {
      c(
        "# this function was generated automatically. do not edit by hand!",
        x,
        rep("", 3)
      )
    }))
  )

  writeLines(text = lines, con = target_script)
}

generated_function_header <- function() {
  # @codedoc_comment_block news("dbc", "2023-08-16", "0.4.16")
  # All generated assertion, report and test functions now check whether
  # `x` is missing and raise an informative error. There were edge cases
  # where `x` was attempted to be evaluated only in a call to `eval` which
  # resulted in cryptic error messages --- now those can no longer occur.
  # @codedoc_comment_block news("dbc", "2023-08-16", "0.4.16")
  c(
    "x_nm <- dbc::handle_arg_x_nm(x_nm)",
    "call <- dbc::handle_arg_call(call)",
    "if (missing(x)) {",
    "  stop(simpleError(",
    "    message = paste0(",
    "      \"Argument \", x_nm, \" was missing --- it has no default so \",",
    "      \"some value must be supplied!\"",
    "    ),",
    "    call = call",
    "  ))",
    "}"
  )
}

generate_report_derivative_funs <- function(
  source_scripts = c(
    "R/generated_report_funs.R"
  ),
  target_script = "R/generated_assertion_funs.R",
  type = c("assert", "test")[1],
  assertion_type = "general"
) {
  stopifnot(
    type %in% c("assert", "test"),
    length(type) == 1L,

    length(assertion_type) == 1L, is.character(assertion_type)
  )
  fun_env <- new.env()
  invisible(lapply(source_scripts, function(script_path) {
    source(script_path, local = fun_env)
  }))

  obj_nms <- ls(envir = fun_env)
  is_report_fun_nm <- grepl(
    pattern = "^report_[_.a-zA-Z0-9]+",
    x = obj_nms
  )
  report_fun_nms <- gsub("\\Q <- \\E.+", "", obj_nms[is_report_fun_nm])
  if (type == "assert" && assertion_type != "general") {
    fun_nms <- sub(
      "^report_", paste0(type, "_", assertion_type, "_"), report_fun_nms
    )
  } else {
    fun_nms <- sub(
      "^report_", paste0(type, "_"), report_fun_nms
    )
  }

  fun_df <- data.frame(fun_nm = fun_nms, report_fun_nm = report_fun_nms)
  # @codedoc_comment_block news("dbc", "2023-07-04", "0.4.15")
  # All generated assertion functions now pass `call` to
  # `dbc::report_to_assertion` arg `raise_error_call`. Therefore, now an error
  # message begins with the call of the guilty function instead of the
  # assertion function.
  # @codedoc_comment_block news("dbc", "2023-07-04", "0.4.15")
  assertion_body_tail <- c(
    "dbc::report_to_assertion(",
    "  report_df,",
    "  assertion_type = assertion_type,",
    "  raise_error_call = call",
    ")",
    "return(invisible(NULL))"
  )
  body_tail <- switch(
    type,
    assert = switch(
      assertion_type,
      general = assertion_body_tail,
      c(
        paste0("assertion_type <- \"", assertion_type, "\""),
        assertion_body_tail
      )
    ),
    test = c(
      "return(all(report_df[[\"pass\"]]))"
    )
  )

  fun_df[["body"]] <- lapply(seq_len(nrow(fun_df)), function(fun_no) {
    report_fun_nm <- report_fun_nms[fun_no]
    arg_nms <- names(formals(fun_env[[report_fun_nm]]))
    body <- paste0("  ", c(
      generated_function_header(),
      paste0("report_df <- dbc::", fun_df[["report_fun_nm"]][fun_no], "("),
      paste0(
        "  ", arg_nms, " = ", arg_nms, c(rep(", ", length(arg_nms) - 1L), "")
      ),
      ")",
      body_tail
    ))
    if (all(c("y_nm", "y") %in% names(formals(fun_env[[report_fun_nm]])))) {
      body <- c(
        body[1:3],
        "is.null(y) # trigger lazy eval",
        "y_nm <- dbc::handle_arg_x_nm(y_nm, arg_nm = \"y\")",
        body[4:length(body)]
      )
    }
    if ("env" %in% names(formals(fun_env[[report_fun_nm]]))) {
      body <- c(
        "  if (is.null(env)) {",
        "    env <- parent.frame(1L)",
        "  }",
        body
      )
    }
    if (report_fun_nm == "report_is") {
      body <- c(
        "  x_test <- tryCatch(",
        "    is.character(x) || is.language(x),",
        "    error = function(e) e,",
        "    warning = function(w) w",
        "  )",
        "  if (!identical(x_test, TRUE)) {",
        "    x <- substitute(x)",
        "  }",
        "",
        body
      )
    }

    if (type == "assert" && assertion_type %in% dev_assertion_types()) {
      body <- c(
        "  if (!dbc::get_dev_mode()) {",
        "    return(invisible(NULL))",
        "  }",
        "",
        body
      )
    } else if (type == "assert" && assertion_type == "general") {
      body <- c(
        "  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)",
        "  if (identical(assertion_type, \"none\")) {",
        "    return(invisible(NULL))",
        "  }",
        "  if (!dbc::get_dev_mode() && assertion_type %in% dev_assertion_types()) {",
        "    return(invisible(NULL))",
        "  }",
        "",
        body
      )
    }
    return(body)
  })

  fun_df[["arg_set"]] <- lapply(report_fun_nms, function(report_fun_nm) {
    formals <- formals(fun_env[[report_fun_nm]])
    if (assertion_type == "general") {
      formals["assertion_type"] <- list(NULL)
    }
    if (type != "assert") {
      formals["assertion_type"] <- NULL
    }
    arg_set <- vapply(
      seq_along(formals),
      function(formal_no) {
        formal_nm <- names(formals)[formal_no]
        formal_default <- paste0(deparse(formals[[formal_no]]), collapse = "")
        ifelse(formal_default %in% c("", NA_character_), formal_nm,
               paste0(formal_nm, " = ", formal_default))
      },
      character(1L)
    )
    arg_set
  })

  fun_df[["def"]] <- lapply(seq_len(nrow(fun_df)), function(fun_no) {
    fun_nm <- fun_df[["fun_nm"]][fun_no]
    assign_line <- paste0(fun_nm, " <- function(")
    body <- fun_df[["body"]][[fun_no]]
    arg_set <- fun_df[["arg_set"]][[fun_no]]
    arg_lines <- paste0("  ", arg_set, c(rep(", ", length(arg_set) - 1L), ""))
    def <- c(
      assign_line,
      arg_lines,
      ") {",
      body,
      "}"
    )
    def
  })

  lines <- unlist(lapply(fun_df[["def"]], function(lines) {
    c(
      rep("", 5),
      "# this function was generated automatically. do not edit by hand!",
      "#' @rdname assertions",
      "#' @export",
      lines
    )
  }))

  lines <- c(
    "# this script was generated automatically. do not edit by hand!",
    lines, rep("", 5)
  )

  lines <- sub("[ ]+$", "", lines)

  writeLines(lines, con = target_script)

}

generate_test_funs <- function(
  source_scripts = c(
    "R/generated_report_funs.R"
  ),
  target_script = "R/generated_assertion_funs.R"
) {
  generate_report_derivative_funs(
    source_scripts = source_scripts,
    target_script = target_script,
    type = "test"
  )
}

generate_assertion_funs <- function(
  source_scripts = c(
    "R/generated_report_funs.R"
  ),
  target_script = "R/generated_test_funs.R",
  assertion_type = "general"
) {
  generate_report_derivative_funs(
    source_scripts = source_scripts,
    target_script = target_script,
    type = "assert",
    assertion_type = assertion_type
  )
}

report_function_variant_space <- function() {
  requireNamespace("data.table")

  levels <- list(
    class = c(
      "double", "number", "integer", "Date", "character", "logical",  "factor"
    ),
    na_status = c("nonNA", ""),
    number_range = c("gtezero", "gtzero", "ltezero", "ltzero", ""),
    storage_type = c("atom", "vector", "matrix")
  )
  fun_nm_dt <- do.call(data.table::CJ, levels)
  data.table::setkeyv(fun_nm_dt, names(fun_nm_dt))
  non_number_types <- c("character", "logical", "Date", "factor")
  fun_nm_dt <- fun_nm_dt[
    !(fun_nm_dt[["class"]] %in% non_number_types &
        fun_nm_dt[["number_range"]] != ""),
  ]
  data.table::setDT(fun_nm_dt)

  fun_def_prefix <- "report_is_"
  fun_nms <- vapply(seq_len(nrow(fun_nm_dt)), function(i) {
    paste0(setdiff(unlist(fun_nm_dt[i, ]), ""), collapse = "_")
  }, character(1L))
  fun_nms <- paste0(fun_def_prefix, fun_nms)
  data.table::set(fun_nm_dt, j = "fun_nm", value = fun_nms)

  data.table::set(
    fun_nm_dt,
    j = setdiff(names(fun_nm_dt), "fun_nm"),
    value = lapply(setdiff(names(fun_nm_dt), "fun_nm"), function(col_nm) {
      col <- fun_nm_dt[[col_nm]]
      fun_nms <- paste0(fun_def_prefix, col)
      fun_calls <- paste0(fun_nms, "(x = x, x_nm = x_nm, call = call)")
      fun_calls[col == ""] <- ""
      fun_calls
    })
  )
  return(fun_nm_dt[])
}



