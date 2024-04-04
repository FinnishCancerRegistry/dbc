s1 <- system2("git", "status", stdout = TRUE)
stopifnot(
  "nothing to commit, working tree clean" %in% s1
)

unlink(
  dir("R/", pattern = "^generated_", full.names = TRUE),
  force = TRUE
)

suppressWarnings(pkgload::load_all(export_all = TRUE))
expression_df <- dbc:::get_report_fun_df()
manually_written_report_df <- local({
  report_fun_scripts <- c(
    "R/manually_written_report_funs.R",
    "R/expr_based.R"
  )
  e <- new.env()
  lapply(report_fun_scripts, function(r_script_path) {
    source(r_script_path, local = e)
  })
  obj_nms <- ls(e)
  df <- data.frame(report_fun_nm = obj_nms[grepl("^report_", obj_nms)])
  df[["extra_arg_set"]] <- lapply(df[["report_fun_nm"]], function(fun_nm) {
    args <- formals(e[[fun_nm]])
    extra_args <- args[!names(args) %in% c("x", "x_nm", "call")]
    if (length(extra_args) == 0) {
      return(NULL)
    }
    out <- vapply(extra_args, deparse1, character(1))
    names(out) <- names(extra_args)
    return(out)
  })
  df
})
invisible(lapply(c("report", "test", "assertion"), function(fun_type) {
  message("* generating fun_type = ", deparse(fun_type),
          " funs...")
  assertion_types <- list(NULL)
  if (fun_type == "assertion") {
    assertion_types <- dbc::assertion_types()
    assertion_types <- setdiff(assertion_types, "none")
  }
  lapply(assertion_types, function(assertion_type) {
    if (!is.null(assertion_type)) {
      message("* generating assertion_type = ", deparse(assertion_type),
              " funs...")
    }
    script_path <- paste0(
      "R/generated_", assertion_type, "_", fun_type, "_funs.R"
    )
    script_path <- gsub("_+", "_", script_path)
    dbc:::generate_script_from_expressions(
      tgt_script_path = script_path,
      df = expression_df,
      fun_type = fun_type,
      assertion_type = assertion_type
    )
    if (fun_type == "report") {
      return(NULL)
    }
    script_path <- paste0(
      "R/generated_",
      assertion_type,
      "_",
      fun_type,
      "_report_function_wrappers.R"
    )
    script_path <- gsub("_+", "_", script_path)
    dbc:::generate_report_function_wrapper_script(
      tgt_script_path = script_path,
      report_fun_nms = paste0(
        "dbc::", manually_written_report_df[["report_fun_nm"]]
      ),
      extra_arg_sets = manually_written_report_df[["extra_arg_set"]],
      fun_type = fun_type,
      assertion_type = assertion_type
    )
  })
}))

devtools::document()

Sys.sleep(5)
s2 <- system2("git", "status", stdout = TRUE)
if (!identical(s1, s2)) {
  system2("git", c("add", "-A"))
  system2("git", c("commit", "-m", "\"feat: run dev/02_generate_functions.R\""))
}
