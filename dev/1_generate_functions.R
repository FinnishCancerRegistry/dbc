

stopifnot(
  vapply(git2r::status(), length, 1L) == 0L
)

s1 <- git2r::status()
e <- new.env()
source("data-raw/sysdata.R", local = e)

unlink(
  dir("R/", pattern = "^generated_", full.names = TRUE),
  force = TRUE
)

pkgload::load_all(export_all = TRUE)
dbc:::generate_report_funs(
  target_script = "R/generated_report_funs.R"
)

report_fun_scripts <- c(
  "R/generated_report_funs.R",
  "R/manually_written_report_funs.R",
  "R/expr_based.R"
)

assertion_types <- dbc::assertion_types()
assertion_types <- setdiff(assertion_types, "none")
invisible(lapply(assertion_types, function(assertion_type) {
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

s2 <- git2r::status()
if (!identical(s1, s2)) {
  git2r::add(path = ".")
  git2r::commit(message = "feat: run dev/1_generate_functions.R")
}
