
# @codedoc_comment_block news("dbc::report_data_table_has_no_duplicates", "2023-04-01", "0.4.11")
# New fun `dbc::report_data_table_has_no_duplicates` and derivatives.
# @codedoc_comment_block news("dbc::report_data_table_has_no_duplicates", "2023-04-01", "0.4.11")
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
