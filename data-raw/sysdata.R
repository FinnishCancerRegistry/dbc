
# @codedoc_comment_block news("dbc::report_data_table_has_no_duplicates", "2023-04-01", "0.4.11")
# New fun `dbc::report_data_table_has_no_duplicates` and derivatives.
# @codedoc_comment_block news("dbc::report_data_table_has_no_duplicates", "2023-04-01", "0.4.11")
# @codedoc_comment_block news("dbc::report_is_data_frame", "2024-01-18", "0.5.0")
# New fun `dbc::report_data_table_has_no_duplicates` and derivatives.
# @codedoc_comment_block news("dbc::report_is_data_frame", "2024-01-18", "0.5.0")
report_fun_specs <- read.table(
  file = "data-raw/report_fun_specs.csv",
  sep = ";",
  quote = "\"",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  blank.lines.skip = TRUE
)
report_fun_specs <- as.data.frame(lapply(report_fun_specs, function(col) {
  col <- as.character(col)
  gsub("'", "\"", col)
}))

pkgload::load_all(export_all = FALSE)
report_df_template <- dbc::expressions_to_report("1 == 1")

r_script_paths <- dir("data-raw/function_chunks/", full.names = TRUE)
function_chunks <- lapply(r_script_paths, function(r_script_path) {
  lines <- suppressWarnings(readLines(r_script_path, encoding = "UTF-8"))
  lines <- lines[!grepl("^[ ]*#+[ ]*", lines)]
  return(lines)
})
names(function_chunks) <- gsub(
  "[.][rR]$",
  "",
  dir("data-raw/function_chunks/", full.names = FALSE)
)

usethis::use_data(
  report_fun_specs,
  report_df_template,
  function_chunks,
  internal = TRUE,
  overwrite = TRUE
)