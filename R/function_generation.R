get_report_fun_specs <- function() {
  report_fun_specs
}

get_report_function_variant_space <- function() {
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

get_report_fun_df <- function() {
  requireNamespace("data.table")
  specs_df <- get_report_fun_specs()
  specs_df_col_nms <- c("test_set_nm", "call",
                        "fail_message", "pass_message",
                        "extra_arg_set")
  raise_internal_error_if_not(
    is.data.frame(specs_df),
    specs_df_col_nms %in% names(specs_df)
  )
  specs_df[, names(specs_df)] <- lapply(specs_df, as.character)
  base_prefix <- "report_"
  fun_df <- data.table::copy(specs_df)

  deriv_fun_df <- get_report_function_variant_space()
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
    fun_df[, c("call", "fail_message", "pass_message", "extra_arg_set")],
    by = list(fun_nm = fun_df[["fun_nm"]], test_set_nm = fun_df[["test_set_nm"]]),
    FUN = function(x) list(unlist(x))
  )
  data.table::setnames(
    fun_df,
    c("call", "fail_message", "pass_message", "test_set_nm"),
    c("expression_set", "fail_message_set", "pass_message_set", "fun_nm_suffix")
  )

  fun_df[["extra_arg_set"]] <- lapply(
    fun_df[["extra_arg_set"]],
    function(x) {
      set <- setdiff(unlist(strsplit(x, ", *")), NA_character_)
      if (length(set) == 0) {
        return(NULL)
      }
      split_set <- strsplit(set, split = "[ ]*=[ ]*")
      set <- vapply(split_set, utils::tail, character(1L), n = 1L)
      names(set) <- vapply(split_set, utils::head, character(1L), n = 1L)
      set[set == names(set)] <- ""
      return(set)
    }
  )

  return(fun_df)
}

get_generated_function_chunk <- function(chunk_name) {
  stopifnot(chunk_name %in% names(function_chunks))
  function_chunks[[chunk_name]]
}

generated_function_header <- function() {
  get_generated_function_chunk("body_start")
}



generate_function_start <- function(
  fun_nm,
  fun_type,
  assertion_type = NULL,
  extra_args = NULL
) {
  stopifnot(
    fun_type %in% c("report", "test", "assertion"),
    is.null(extra_args) || is.character(extra_args)
  )
  if (is.character(extra_args)) {
    if (length(extra_args) == 0) {
      extra_args <- NULL
    } else {
      stopifnot(
        !is.null(names(extra_args)),
        !"" %in% names(extra_args)
      )
    }
  }
  if (fun_type == "assertion") {
    stopifnot(assertion_type %in% dbc::assertion_types())
    if (assertion_type == "general") {
      extra_args <- c(
        "assertion_type" = "NULL",
        extra_args
      )
    }
  } else {
    stopifnot(is.null(assertion_type))
  }
  lines <- paste0(fun_nm, " <- function(")
  arg_lines <- get_generated_function_chunk("args_start")
  if (length(extra_args) > 0) {
    arg_lines <- c(
      arg_lines,
      paste0(names(extra_args), " = ", extra_args, ",")
    )
    arg_lines <- gsub(" = ,", ",", arg_lines)
  }
  arg_lines[length(arg_lines)] <- sub(",$", "", arg_lines[length(arg_lines)])
  lines <- c(
    lines,
    paste0("  ", arg_lines),
    ") {"
  )
  return(lines)
}

#' @title Function Generation
#' @description
#' Generate report, test, and assertion functions.
#' @name function_generation
NULL

#' @rdname function_generation
#' @export
#' @param fun_nm `[character]` (no default)
#' 
#' Name of function to generate.
#' @param fun_type `[character]` (no default)
#' 
#' One of `c("test", "report", "assertion")`.
#' @param expressions `[character]` (no default)
#' 
#' One or more R expressions as `character` strings. These will be evaluted
#' in the given order in the generated function.
#' @param assertion_type `[NULL, character]` (default `NULL`)
#' 
#' If `fun_type == "assertion"`, this must be one of the alternatives given
#' by `[assertion_types]`.
#' @param fail_messages `[NULL, character]` (default `NULL`)
#' 
#' If `fun_type != "test"`, this cannot be `NULL` but must give a failure
#' message for each `expressions` element --- though `NA_character_` is allowed.
#' `NA_character_` will be replaced internally by a generic message.
#' @param pass_messages `[NULL, character]` (default `NULL`)
#' 
#' If `fun_type == "report"`, see `[expressions_to_report]`. Else ignored.
#' @param extra_args `[NULL, character]` (default `NULL`)
#' 
#' - `NULL`: The function will take no extra arguments.
#' - `character`: The function will take extra arguments corresponding to the
#'   names of this object, and their defaults will be the elements of the
#'   object. E.g. `c(my_extra_arg = "NULL")`.
generate_function_from_expressions <- function(
  fun_nm,
  fun_type,
  expressions,
  assertion_type = NULL,
  fail_messages = NULL,
  pass_messages = NULL,
  extra_args = NULL
) {
  # @codedoc_comment_block news("dbc::generate_function_from_expressions", "2024-01-11", "0.5.0")
  # New exported function `dbc::generate_function_from_expressions`. It is
  # used to produce all generated functions in `dbc` that are based on
  # expressions.
  # @codedoc_comment_block news("dbc::generate_function_from_expressions", "2024-01-11", "0.5.0")

  lines <- generate_function_start(
    fun_nm = fun_nm,
    fun_type = fun_type,
    assertion_type = assertion_type,
    extra_args = extra_args
  )
  if (fun_type == "report") {
    body_lines <- generate_report_function_body_from_expressions(
      expressions = expressions,
      fail_messages = fail_messages,
      pass_messages = pass_messages
    )
  } else if (fun_type == "test") {    
    body_lines <- generate_test_function_body_from_expressions(
      expressions = expressions
    )
  } else if (fun_type == "assertion") {    
    body_lines <- generate_assertion_function_body_from_expressions(
      expressions = expressions,
      fail_messages = fail_messages,
      assertion_type = assertion_type
    )
  }

  lines <- c(
    lines,
    paste0("  ", body_lines),
    "}" 
  )
  return(lines)
}

generate_report_function_body_from_expressions <- function(
  expressions,
  fail_messages,
  pass_messages
) {
  # @codedoc_comment_block news("dbc", "2023-07-04", "0.4.15")
  # All generated assertion functions now pass `call` to
  # `dbc::report_to_assertion` arg `raise_error_call`. Therefore, now an error
  # message begins with the call of the guilty function instead of the
  # assertion function.
  # @codedoc_comment_block news("dbc", "2023-07-04", "0.4.15")

  stopifnot(    
    is.character(expressions),
    is.character(fail_messages),
    is.character(pass_messages),
    length(expressions) == length(fail_messages),
    length(expressions) == length(pass_messages)
  )

  lines <- c(
    get_generated_function_chunk("body_start"),
    "",
    "expressions <- ",
    paste0("  ", deparse1(expressions)),
    "fail_messages <- ",
    paste0("  ", deparse1(fail_messages)),
    "pass_messages <- ",
    paste0("  ", deparse1(pass_messages)),
    "report_env <- environment()",  
    "report_df <- dbc::expressions_to_report(",
    "  expressions = expressions,",
    "  fail_messages = fail_messages,",
    "  pass_messages = pass_messages,",
    "  env = report_env, ",
    "  call = call",
    ")",
    "return(report_df)"
  )
  return(lines)
}

generate_assertion_function_body_start <- function(
  assertion_type
) {
  stopifnot(
    assertion_type %in% dbc::assertion_types(),
    length(assertion_type) == 1
  )
  if (assertion_type == "general") {
    append <- "assertion_type <- dbc::handle_arg_assertion_type(assertion_type)"
  } else {
    append <- sprintf("assertion_type <- \"%s\"", assertion_type)
  }
  c(
    get_generated_function_chunk("body_start"),
    "",
    append,
    ""
  )
}

generate_assertion_function_body_from_expressions <- function(
  expressions,
  fail_messages,
  assertion_type
) {
  # @codedoc_comment_block news("dbc", "2024-01-11", "0.5.0")
  # All generated assertion functions no longer create reports and pass those
  # to `dbc::report_to_assertion`. Instead generated assertion functions use
  # `stop` directly. This reduced wall clock time used in evaluation by ~90%.
  # @codedoc_comment_block news("dbc", "2024-01-11", "0.5.0")
  stopifnot(
    is.character(expressions),
    is.character(fail_messages),
    length(expressions) == length(fail_messages),
    !grepl("^\"", fail_messages),
    !grepl("\"$", fail_messages),
    assertion_type %in% dbc::assertion_types()
  )

  lines <- c(
    generate_assertion_function_body_start(
      assertion_type = assertion_type
    ),
    unlist(lapply(seq_along(expressions), function(i) {
      lines <- c(
        "dbc::assertion_eval(",
        "  expression = quote(EXPRESSION),",
        "  fail_message = FAIL_MESSAGE,",
        "  assertion_type = assertion_type,",
        "  x_nm = x_nm,",
        "  call = call",
        ")"
      )
      replace <- c(
        "EXPRESSION" = expressions[i],
        "FAIL_MESSAGE" = paste0("\"", fail_messages[i], "\"")
      )
      for (re in names(replace)) {
        lines <- gsub(re, replace[re], lines)
      }
      c(lines, "")
    })) 
  )

  return(lines)
}

generate_test_function_body_from_expressions <- function(
  expressions
) {
  stopifnot(
    is.character(expressions)
  )

  lines <- c(
    get_generated_function_chunk("body_start"),
    "",
    "out <- TRUE",
    "",
    unlist(lapply(seq_along(expressions), function(i) {
      lines <- get_generated_function_chunk("test_eval")
      replace <- c(
        "EXPRESSION" = expressions[i]
      )
      for (re in names(replace)) {
        lines <- gsub(re, replace[re], lines)
      }
      c(lines, "")
    })) 
  )
  return(lines)
}

generate_report_function_call <- function(
  report_fun_nm,
  extra_arg_nms = NULL
) {
  report_fun_args <- c(   
    "x",
    "x_nm",
    "call"
  )
  report_fun_args <- union(report_fun_args, extra_arg_nms)
  report_fun_arg_lines <- paste0(
    report_fun_args, " = ", report_fun_args, ","
  )
  report_fun_arg_lines[length(report_fun_arg_lines)] <- sub(
    ",$",
    "",
    report_fun_arg_lines[length(report_fun_arg_lines)]
  )
  report_fun_arg_lines <- paste0("  ", report_fun_arg_lines)
  lines <- c(
    "report_df <- REPORT_FUN(",
    report_fun_arg_lines,
    ")"
  )
  
  replacements <- c(
    "REPORT_FUN" = report_fun_nm
  )
  for (re in names(replacements)) {
    lines <- gsub(re, replacements[re], lines)
  }

  return(lines)
}

generate_assertion_function_body_to_wrap_report_function <- function(
  report_fun_nm,
  assertion_type,
  extra_arg_nms = NULL
) {
  lines <- c(
    generate_assertion_function_body_start(
      assertion_type = assertion_type
    ),
    generate_report_function_call(
      report_fun_nm = report_fun_nm,
      extra_arg_nms = extra_arg_nms
    ),
    "dbc::report_to_assertion(",
    "  report_df = report_df,",
    "  assertion_type = assertion_type,",
    "  raise_error_call = call",
    ")"
  )
  return(lines)
}

generate_test_function_body_to_wrap_report_function <- function(
  report_fun_nm,
  extra_arg_nms = NULL
) {
  lines <- c(
    get_generated_function_chunk("body_start"),
    generate_report_function_call(
      report_fun_nm = report_fun_nm,
      extra_arg_nms = extra_arg_nms
    ),
    "return(all(report_df[[\"pass\"]]))"
  )
  return(lines)
}

#' @rdname function_generation
#' @param report_fun_nm `[character]` (no default)
#' 
#' Name of function to generate wrapper for --- e.g. `"dbc::report_is_integer"`.
#' @export 
generate_report_function_wrapper <- function(
  report_fun_nm,
  fun_nm,
  fun_type,
  assertion_type = NULL,
  extra_args = NULL
) {
  # @codedoc_comment_block news("dbc::generate_report_function_wrapper", "2024-01-12", "0.5.0")
  # New exported function `dbc::generate_report_function_wrapper`.
  # This function is used to generate all report function wrappers in `dbc`.
  # @codedoc_comment_block news("dbc::generate_report_function_wrapper", "2024-01-12", "0.5.0")
  stopifnot(
    fun_type %in% c("test", "assertion")
  )
  lines <- generate_function_start(
    fun_nm = fun_nm,
    fun_type = fun_type,
    assertion_type = assertion_type,
    extra_args = extra_args
  )
  if (fun_type == "test") {
    body_lines <- generate_test_function_body_to_wrap_report_function(
      report_fun_nm = report_fun_nm,
      extra_arg_nms = names(extra_args)
    )
  } else {
    body_lines <- generate_assertion_function_body_to_wrap_report_function(
      report_fun_nm = report_fun_nm,
      extra_arg_nms = names(extra_args),
      assertion_type = assertion_type
    )
  }
  lines <- c(
    lines,
    paste0("  ", body_lines),
    "}"
  )
  return(lines)
}

#' @rdname function_generation
#' @export
generate_script_from_expressions <- function(
  tgt_script_path,
  df,
  fun_type,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("dbc::generate_script_from_expressions", "2024-01-12", "0.5.0")
  # New exported function `dbc::generate_script_from_expressions`.
  # This function is used to produce all generated expression-based functions
  # in `dbc`.
  # @codedoc_comment_block news("dbc::generate_script_from_expressions", "2024-01-12", "0.5.0")
  exp_col_nms <- c(
    "fun_nm_suffix",
    "expression_set",
    "fail_message_set",
    "pass_message_set",
    "extra_arg_set"
  )
  stopifnot(
    #' @param df `[data.frame]` (no default)
    #' 
    #' `data.frame` of metadata on functions to generate. Columns:
    #'  - `fun_nm_suffix`: These will be the suffixes of generated function
    #'    names. E.g. `"has_length_one"`.
    #'  - `expression_set`: A list column, where each element is a vector of
    #'    character strings of R expressions. These will be the actual tests
    #'    performed. See
    #'    `expressions` of `dbc::generate_function_from_expressions`.
    #'  - `fail_message_set`: This list column contains one `fail_message`
    #'    per corresponding expression in `expression_set`. See
    #'    `fail_messages` of `dbc::generate_function_from_expressions`.
    #'  - `pass_message_set`: This list column contains one `pass_message`
    #'    per corresponding expression in `expression_set`. See
    #'    `pass_messages` of `dbc::generate_function_from_expressions`.
    #'  - `extra_arg_set`: This list column contains a character string vector
    #'    or `NULL` for every function. See arg `extra_args` of
    #'    `dbc::generate_function_from_expressions`.
    #' 
    #' 
    is.data.frame(df),
    exp_col_nms %in% names(df),

    #' @param tgt_script_path `[character]` (no default)
    #' 
    #' Path to script to be generated. Will be overwritten if it exists.
    is.character(tgt_script_path),
    length(tgt_script_path) == 1,
    dir.exists(dirname(tgt_script_path))
  )

  lines <- unlist(lapply(seq_len(nrow(df)), function(i) {
    fun_nm_prefix <- sub("assertion", "assert", fun_type)
    if (!is.null(assertion_type) && !assertion_type %in% c("general", "none")) {
      fun_nm_prefix <- paste0(fun_nm_prefix, "_", assertion_type)
    }
    fun_nm <- paste0(
      fun_nm_prefix,
      "_",
      df[["fun_nm_suffix"]][i]
    )
    lines_i <- dbc::generate_function_from_expressions(
      fun_nm = fun_nm,
      fun_type = fun_type,
      assertion_type = assertion_type,
      expressions = df[["expression_set"]][[i]],
      fail_messages = df[["fail_message_set"]][[i]],
      pass_messages = df[["pass_message_set"]][[i]],
      extra_args = df[["extra_arg_set"]][[i]]
    )
    lines_i <- c(
      "# generated by dbc::generate_script_from_expressions.",
      "# do no modify by hand!",
      "#' @rdname assertions",
      "#' @export",
      lines_i,
      ""
    )
    return(lines_i)
  }))
  lines <- c(
    "# generated by dbc::generate_script_from_expressions.",
    "# do no modify by hand!",
    "",
    lines
  )
  writeLines(lines, tgt_script_path)
}

#' @rdname function_generation
#' @export
generate_report_function_wrapper_script <- function(
  tgt_script_path,
  report_fun_nms,
  extra_arg_sets,
  fun_type,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("dbc::generate_report_function_wrapper_script", "2024-01-12", "0.5.0")
  # New exported function `dbc::generate_report_function_wrapper_script`.
  # This function is used to produce all generated report wrapper functions
  # in `dbc`.
  # @codedoc_comment_block news("dbc::generate_report_function_wrapper_script", "2024-01-12", "0.5.0")
  stopifnot(
    #' @param report_fun_nms `[character]` (no default)
    #' 
    #' One or more report function names. Each in turn passed to
    #' `dbc::generate_report_function_wrapper` arg `report_fun_nm`.
    is.character(report_fun_nms),

    #' @param extra_arg_sets `[list]` (no default)
    #' 
    #' A set of of `extra_args` for each element of `report_fun_nms` ---
    #' each `NULL` or a character string vector.
    inherits(extra_arg_sets, "list"),
    length(extra_arg_sets) == length(report_fun_nms),
    vapply(extra_arg_sets, inherits, logical(1L), what = c("NULL", "character"))
  )
  lines <- unlist(lapply(seq_along(report_fun_nms), function(i) {
    fun_nm_prefix <- sub("assertion", "assert", fun_type)
    if (!is.null(assertion_type) && !assertion_type %in% c("general", "none")) {
      fun_nm_prefix <- paste0(fun_nm_prefix, "_", assertion_type)
    }
    fun_nm <- sub("[a-zA-Z_.-]+:+", "", report_fun_nms[i])
    fun_nm <- sub("^report", fun_nm_prefix, fun_nm)
    lines_i <- generate_report_function_wrapper(
      report_fun_nm = report_fun_nms[i],
      fun_nm = fun_nm,
      fun_type = fun_type,
      assertion_type = assertion_type,
      extra_args = extra_arg_sets[[i]]
    )
    lines_i <- c(
      "# generated by dbc::generate_report_function_wrapper_script.",
      "# do no modify by hand!",
      "#' @rdname assertions",
      "#' @export",
      lines_i,
      ""
    )
    return(lines_i)
  }))
  lines <- c(
    "# generated by dbc::generate_report_function_wrapper_script.",
    "# do no modify by hand!",
    "",
    lines
  )
  writeLines(lines, tgt_script_path)
}
