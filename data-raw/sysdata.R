

report_fun_specs <- read.table(
  file = "data-raw/report_fun_specs.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  blank.lines.skip = TRUE
)
report_fun_specs <- as.data.frame(lapply(report_fun_specs, as.character))

usethis::use_data(report_fun_specs, internal = TRUE, overwrite = TRUE)
