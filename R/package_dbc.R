#' @name dbc
#' @docType package
#' @title dbc: Functions To Aid Design By Contract
#'
#' @eval c(
#'   codedoc::codedoc_R_package_description("dbc"),
#'   codedoc::codedoc_news_for_R_package()
#' )
NULL

# @codedoc_comment_block R_package_example(dbc)
# # by adding arg assertion_type, you can use the same function for end-user
# # purposes and internal purposes with clear error messages.
# my_fun <- function(df, by, assertion_type = NULL) {
#   if (is.null(assertion_type)) {
#     assertion_type <- dbc::assertion_type_default()
#   }
#   dbc::assert_is_character_nonNA_vector(
#     x = by,
#     assertion_type = assertion_type
#   )
#   dbc::assert_is_data.frame_with_required_names(
#     x = df,
#     required_names = by,
#     assertion_type = assertion_type
#   )
#   return(table(df[,by]))
# }
# my_fun(df, c("var_1", "var_2"))
# my_fun_2 <- function(df) {
#   my_fun(df, c("var_1", "var_2"), assertion_type = "prod_input")
# }
# @codedoc_comment_block R_package_example(dbc)

# @codedoc_comment_block R_package_description(dbc)
# `dbc` is designed to aid writing functions under the design by contract
# philosophy, where function inputs and outputs are programmatically
# asserted to adhere to specifications.
#
# <!-- badges: start -->
# [![R-CMD-check](https://github.com/WetRobot/dbc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WetRobot/dbc/actions/workflows/R-CMD-check.yaml)
# <!-- badges: end -->
#
# # Recommended installation
#
# ```r
# devtools::install_github(
#   "FinnishCancerRegistry/dbc",
#   ref = readline("enter latest tag on github: ")
# )
# ```
#
# # Example
# ```r
# @codedoc_insert_comment_block R_package_example(dbc)
# ```
#
# @codedoc_comment_block R_package_description(dbc)

# @codedoc_comment_block news("dbc::report_has_class", "2022-07-21", "0.4.8")
# @codedoc_comment_block news("dbc::report_inherits", "2022-07-21", "0.4.8")
#
# `dbc::report_has_class` and `dbc::report_inherits` error message improved for
# when `required_class` is improper.
#
# @codedoc_comment_block news("dbc::report_inherits", "2022-07-21", "0.4.8")
# @codedoc_comment_block news("dbc::report_has_class", "2022-07-21", "0.4.8")
