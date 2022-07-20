

e <- new.env()
source("data-raw/sysdata.R", local = e)

pkgload::load_all(export_all = TRUE)
dbc:::generate_report_funs(
  target_script = "R/generated_report_funs.R"
)

report_fun_scripts <- c(
  "R/generated_report_funs.R",
  "R/manually_written_report_funs.R",
  "R/expr_based.R"
)


invisible(lapply(dbc::assertion_types(), function(assertion_type) {
  message("* generating assertion_type = ", deparse(assertion_type),
          " funs...")
  dbc:::generate_assertion_funs(
    source_scripts = report_fun_scripts,
    target_script = paste0("R/generated_", assertion_type, "_assertion_funs.R"),
    assertion_type = assertion_type
  )
  NULL
}))


dbc:::generate_test_funs(
  source_scripts = report_fun_scripts,
  target_script = "R/generated_test_funs.R"
)

devtools::document()
