

#' @name dbc
#' @docType package
#' @title dbc: Functions To Aid Design By Contract
#' @description
#' `dbc` is designed to aid writing functions under the design by contract
#' philosophy, where function inputs and outputs are programmatically
#' asserted to adhere to specifications.
#'
#' @details
#' Recommended ways of using assertions:
#'
#' ```
#' # function intended only for use in other functions
#' my_workhorse_fun <- function(x) {
#'   dbc::assert_prod_input_is_number(x)
#'   # ... something complicated that makes assertion on output actually
#'   # worth having
#'   dbc::assert_prod_output_is_number(output)
#'   return(output)
#' }
#' # function exposed to user
#' my_fun <- function(x) {
#'   dbc::assert_user_input_is_number(x)
#'   # some steps...
#'   output <- my_workhorse_fun(x = x)
#'   # some steps...
#'   return(output)
#' }
#' ```
#'
#' Alternative:
#' ```
#' # function used internally & exposed to user
#' my_fun <- function(x, assertion_type = "input") {
#'   dbc::assert_is_number(x, assertion_type = assertion_type)
#'   # some steps...
#'   return(output)
#' }
#' ```
#'
#' Writing your own assertions should done using `[dbc::expressions_to_report]`
#' and `[dbc::report_to_assertion]` when possible. Write a separate report
#' function first.
#'
#' ```
#' report_is_my_arg <- function(x, x_nm = NULL, call = NULL) {
#'   x_nm <- dbc::handle_arg_x_nm(x_nm)
#'   call <- dbc::handle_arg_x_nm(call)
#'   dbc::expressions_to_report(
#'     expressions = "x %in% 1:5",
#'     fail_messages = paste0(x_nm, " was not in set 1:5"),
#'     call = call
#'   )
#' }
#'
#' assert_is_my_arg <- function(
#'   x, x_nm = NULL, call = NULL, assertion_type = "input"
#' ) {
#'   x_nm <- dbc::handle_arg_x_nm(x_nm)
#'   call <- dbc::handle_arg_x_nm(call)
#'   dbc::report_to_assertion(report_is_my_arg(x, x_nm, call),
#'                            assertion_type = assertion_type)
#' }
#' ```
NULL


