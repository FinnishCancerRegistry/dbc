

report_fun_specs <- read.table(
  file = "data-raw/report_fun_specs.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  blank.lines.skip = TRUE
)
report_fun_specs <- as.data.frame(lapply(report_fun_specs, as.character))

pkgload::load_all(export_all = FALSE)
report_df_template <- dbc::expressions_to_report("1 == 1")

usethis::use_data(
  report_fun_specs, report_df_template, internal = TRUE, overwrite = TRUE
)
