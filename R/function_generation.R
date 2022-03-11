









get_report_fun_specs <- function() {
  report_fun_specs
}



#' @importFrom stats aggregate
generate_base_report_funs <- function(
  target_script = "R/generated_base_report_funs.R"
) {
  specs_df <- get_report_fun_specs()
  raise_internal_error_if_not(
    is.data.frame(specs_df),
    c("test_set_nm", "call",
      "fail_message", "pass_message",
      "extra_arg_nm_set"
    ) %in% names(specs_df),

    is.character(target_script),
    length(target_script) == 1L
  )
  specs_df[, names(specs_df)] <- lapply(specs_df, as.character)
  base_prefix <- "report_"
  fun_df <- data.frame(suffix = sort(unique(specs_df[["test_set_nm"]])))
  fun_df[["suffix"]] <- as.character(fun_df[["suffix"]])
  fun_df[["nm"]] <- paste0(base_prefix, fun_df[["suffix"]])

  test_set_nm_set <- fun_df$suffix
  fun_df[["extra_arg_set"]] <- lapply(test_set_nm_set, function(test_set_nm) {
    is_test_set <- specs_df[["test_set_nm"]] == test_set_nm
    values <- specs_df[["extra_arg_nm_set"]][is_test_set]
    paste0(setdiff(values, c(NA_character_, "")), collapse = ", ")
  })
  split_col_nms <- c(
    "test_set" = "call",
    "fail_msg_set" = "fail_message",
    "pass_msg_set" = "pass_message"
  )
  fun_df[, names(split_col_nms)] <- lapply(split_col_nms, function(col_nm) {
    src_col_nm <- col_nm
    lapply(test_set_nm_set, function(test_set_nm) {
      is_test_set <- specs_df[["test_set_nm"]] == test_set_nm
      values <- specs_df[[src_col_nm]][is_test_set]
      paste0(deparse(values), collapse = "")
    })
  })
  fun_df[["body"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    body <- paste0("  ", c(
      "is.null(x) # trigger lazy eval -> no \"restarting interrupted promise evaluation\"",
      "x_nm <- dbc::handle_arg_x_nm(x_nm)",
      "call <- dbc::handle_arg_call(call)",
      "report_env <- environment()",
      "test_set <- c(",
      paste0("  ", fun_df[["test_set"]][fun_no]),
      ")",
      "fail_msg_set <- c(",
      paste0("  ", fun_df[["fail_msg_set"]][fun_no]),
      ")",
      "pass_msg_set <- c(",
      paste0("  ", fun_df[["pass_msg_set"]][fun_no]),
      ")",
      "report_df <- expressions_to_report(",
      "  expressions = test_set,",
      "  fail_messages = fail_msg_set,",
      "  pass_messages = pass_msg_set,",
      "  env = report_env, ",
      "  call = call",
      ")",
      "return(report_df)"
    ))
    body
  })
  fun_df[["fun_def"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    body <- fun_df[["body"]][[fun_no]]
    arg_set <- c("x", "x_nm = NULL", "call = NULL")
    arg_set <- setdiff(
      c(arg_set, fun_df[["extra_arg_set"]][fun_no]),
      c(NA_character_, "")
    )
    arg_set <- paste0(arg_set, collapse = ", ")
    def <- c(
      "#' @rdname assertions",
      "#' @export",
      paste0(fun_df[["nm"]][fun_no], " <- function(", arg_set, ") {"),
      body,
      "}"
    )
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


generate_report_derivative_funs <- function(
  source_scripts = c(
    "R/generated_base_report_funs.R",
    "R/generated_report_fun_variants.R"
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

  body_part <- switch(
    type,
    assert = switch(
      assertion_type,
      general = c(
        "dbc::report_to_assertion(report_df, assertion_type = assertion_type)",
        "return(invisible(NULL))"
      ),
      c(
        paste0("assertion_type <- \"", assertion_type, "\""),
        "dbc::report_to_assertion(report_df, assertion_type = assertion_type)",
        "return(invisible(NULL))"
      )
    ),
    test = c(
      "return(all(report_df[[\"pass\"]]))"
    )
  )


  fun_df[["body"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    report_fun_nm <- report_fun_nms[fun_no]
    arg_nms <- names(formals(fun_env[[report_fun_nm]]))
    paste0("  ", c(
      "is.null(x) # trigger lazy eval -> no \"restarting interrupted promise evaluation\"",
      "x_nm <- dbc::handle_arg_x_nm(x_nm)",
      "call <- dbc::handle_arg_call(call)",
      paste0("report_df <- dbc::", fun_df[["report_fun_nm"]][fun_no], "("),
      paste0(
        "  ", arg_nms, " = ", arg_nms, c(rep(", ", length(arg_nms) - 1L), "")
      ),
      ")",
      body_part
    ))
  })

  fun_df[["arg_set"]] <- lapply(report_fun_nms, function(report_fun_nm) {
    formals <- formals(fun_env[[report_fun_nm]])
    if (assertion_type == "general") {
      formals[["assertion_type"]] <- "general"
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

  fun_df[["def"]] <- lapply(1:nrow(fun_df), function(fun_no) {
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

  writeLines(lines, con = target_script)

}

generate_test_funs <- function(
  source_scripts = c(
    "R/generated_base_report_funs.R",
    "R/generated_report_fun_variants.R"
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
    "R/generated_base_report_funs.R",
    "R/generated_report_fun_variants.R"
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

generate_function_variants <- function(
  prefix = c("report", "assert", "test")[1],
  target_script = "R/generated_report_fun_variants.R",
  pad = rep("", 5)
) {
  requireNamespace("data.table")
  levels <- list(
    c("double", "number", "integer", "Date", "character", "logical",  "factor"),
    "_",
    c("nonNA", ""),
    "_",
    c("gtezero", "gtzero", "ltezero", "ltzero", ""),
    "_",
    c("atom", "vector", "matrix")
  )

  fun_def_prefix <- paste0(prefix, "_is_")

  fun_nm_dt <- do.call(data.table::CJ, levels)
  data.table::setkeyv(fun_nm_dt, names(fun_nm_dt))
  non_number_types <- c("character", "logical", "Date", "factor")
  fun_nm_dt <- fun_nm_dt[
    !(fun_nm_dt[["V1"]] %in% non_number_types &
        fun_nm_dt[["V5"]] != ""),
  ]
  fun_nms <- do.call(paste0, fun_nm_dt)
  fun_nms <- paste0(fun_def_prefix, fun_nms)
  fun_nms <- gsub("_{1,}", "_", fun_nms)

  data.table::set(fun_nm_dt, j = c("V2", "V4", "V6"), value = NULL)
  data.table::set(
    fun_nm_dt,
    j = names(fun_nm_dt),
    value = lapply(fun_nm_dt, function(col) {
      fun_nms <- paste0(fun_def_prefix, col)
      fun_calls <- paste0(fun_nms, "(x = x, x_nm = x_nm, call = call)")
      fun_calls[col == ""] <- ""
      fun_calls
    })
  )

  fun_definitions <- unlist(lapply(seq_along(fun_nms), function(i) {
    fun_nm <- fun_nms[i]
    def <- paste0(fun_nm, " <- function(x, x_nm = NULL, call = NULL) {")
    call_lines <- setdiff(as.character(fun_nm_dt[i, ]), "")
    line_ends <- c(rep(", ", length(call_lines) - 1L), "")
    def <- c(
      def,
      "  x_nm <- dbc::handle_arg_x_nm(x_nm)",
      "  call <- dbc::handle_arg_call(call)",
      "  out <- rbind(",
      paste0("    ", call_lines, line_ends),
      "  )",
      "  return(out)"
    )
    def <- c(def, "}", rep("", 1))
    def <- c(
      "#' @rdname assertions",
      "#' @export",
      def
    )
  }))

  lines <- c(pad, fun_definitions)

  writeLines(text = lines, con = target_script)
  invisible(NULL)
}




