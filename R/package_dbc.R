#' @eval c(
#'   codedoc::codedoc_R_package_description("dbc"),
#'   codedoc::codedoc_news_for_R_package()
#' )
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# @codedoc_comment_block R_package_example(dbc)
# # by adding arg assertion_type, you can use the same function for end-user
# # purposes and internal purposes with clear error messages.
# my_fun <- function(df, by, assertion_type = NULL) {
#   dbc::assert_is_character_nonNA_vector(
#     x = by,
#     assertion_type = assertion_type
#   )
#   dbc::assert_is_data_frame_with_required_names(
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
# [![R-CMD-check](https://github.com/FinnishCancerRegistry/dbc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/dbc/actions/workflows/R-CMD-check.yaml)
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

# @codedoc_comment_block news("dbc", "2023-04-18", "0.4.13")
# Improved `*_vector_elems_are_in_set` funs: assertion failure message
# also shows first ten elems of `set`.
# @codedoc_comment_block news("dbc", "2023-04-18", "0.4.13")

# @codedoc_comment_block news("dbc", "2024-04-18", "0.5.3")
# `dbc::*_is_uniquely_named_*` funs now pass objects of length zero.
# E.g. `dbc::test_is_uniquely_named(list()) == TRUE`.
# @codedoc_comment_block news("dbc", "2024-04-18", "0.5.3")

# @codedoc_comment_block news("dbc", "2024-04-25", "0.5.4")
# `dbc::*_is_nonNA_*` funs now pass all objects that are not vectors (e.g.
# lists, language objects). Only vectors are tested with `is.na`.
# @codedoc_comment_block news("dbc", "2024-04-25", "0.5.4")

# @codedoc_comment_block news("dbc", "2024-04-29", "0.5.5")
# New functions `dbc::*_is_subset_of_data_frame_*` and
# `dbc::*_is_subset_of_data_table_*`.
# @codedoc_comment_block news("dbc", "2024-04-29", "0.5.5")

# @codedoc_comment_block news("dbc", "2024-10-01", "0.6.0")
# `is_lt` / `is_lte` / `is_gt` / `is_gte` functions now ignore `NA` values.
# Formerly `NA` values caused failures even in functions such as
# `dbc::assert_is_integer_gtzero_vector` which should allow them.
# @codedoc_comment_block news("dbc", "2024-10-01", "0.6.0")

# @codedoc_comment_block news("dbc", "2024-10-02", "0.7.0")
# Marked `is_data.frame/table` (snail.case) for future removal.
# New functions `*_is_data_frame_with_required_names`.
# @codedoc_comment_block news("dbc", "2024-10-02", "0.7.0")

# @codedoc_comment_block news("dbc", "2025-07-10", "0.8.0")
# Added `dbc::report_is_unevaluated_expression` and derivatives.
# @codedoc_comment_block news("dbc", "2025-07-10", "0.8.0")
