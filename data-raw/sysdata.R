

report_fun_specs <- read.table(
  file = "data-raw/report_fun_specs.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  blank.lines.skip = TRUE
)
report_fun_specs <- as.data.frame(lapply(report_fun_specs, as.character))
report_fun_specs <- report_fun_specs[
  !grepl("^assert_", report_fun_specs$call),
]

usethis::use_data(report_fun_specs, internal = TRUE, overwrite = TRUE)
