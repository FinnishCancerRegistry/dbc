

#' @name dbc
#' @docType package
#' @title dbc: Functions To Aid Design By Contract
#' @description
#' `dbc` is designed to aid writing functions under the design by contract
#' philosophy, where function inputs and outputs are programmatically
#' asserted to adhere to specifications.
#'
#' @details
#' `dbc` offers the following families of functions:
#'
#' - `test`: e.g. `test_is_integer_atom`; returns `TRUE` / `FALSE`
#' - `report_`: e.g. `report_is_integer_atom`; returns a
#'   `data.frame` with columns `test`, `result`, `pass`, and `message`;
#'   failed test will have an associated error message
#' - `assert_user_input_`: e.g. `assert_user_input_is_integer_atom`;
#'   these specifically check the values of function arguments supplied by the
#'   user and should not be used for other purposes; they emit
#'   a more user-friendly error so the user knows to adjust what they supply
#'   to the function; you don't want to use these functions in internal
#'   functions
#' - `assert_prod_input_`: same as `assert_user_input`, but intended to check
#'   inputs of functions when the user has no direct influence on the inputs;
#'   these functions emit an error useful for debugging and direct the user
#'   to report the error
#' - `assert_dev_input_`: same as `assert_input_`, but only intended to be
#'   executed during development / unit testing; in production these assertions
#'   are not executed
#' - `assert_prod_output_`: same as `assert_prod_input_`, but checks function
#'   output
#' - `assert_dev_output_`: same as `assert_dev_input_`, but checks function
#'   output
#' - `assert_prod_interim_`: same as `assert_prod_input_`, but intended to
#'   check interim results in your function
#' - `assert_dev_interim_`: same as `assert_prod_interim_`, but only executed
#'   in dev mode
#'
#' `assert_dev_input_` and `assert_dev_output_` families assume that the
#' functions are NOT called in development mode by default; they are executed
#' if and only if `dbc::get_dev_mode() == TRUE`; see [set_dev_mode].
#'
#' In summary, `assert_` functions are intended to check that something has
#' not happened which should not happen (something is wrong with the function),
#' `test_` functions return logical values, `report_` return reports
#' which can be used in more customised checking, and `assert_user_input_`
#' functions verify inputs given by the user. `assert_user_input_` cannot know
#' if it is in fact checking inputs not directly given by the user, i.e.
#' the error messages will be poor if a function intended for the user is also
#' used internally. To this end it is recommended to write a separate
#' internal function and a separate function exposed to the user, which only
#' wraps around the internal function. E.g.
#'
#' ```
#' # function intended only for use in other functions
#' my_fun_ <- function(x) {
#'   dbc::assert_prod_input_number(x)
#'   output <- x ^ 2
#'   dbc::assert_prod_output_number(x)
#'   return(output)
#' }
#' # function exposed to user
#' my_fun <- function(x) {
#'   dbc::assert_user_input_number(x)
#'   # some other steps...
#'   my_fun_(x = x)
#' }
#' ```
#'
#' when there are many assertions, you may want to turn them into a function:
#' ```
#' # functions intended only for use in other functions
#' my_fun_input_assertions <- function(x, assertion_type) {
#'   dbc::report_to_assertion(
#'     report_df = rbind(
#'       dbc::report_is_number(x)
#'       # ... more checks
#'     ),
#'     assertion_type = assertion_type
#'   )
#' }
#' my_fun_ <- function(x) {
#'   my_fun_input_assertions(x, assertion_type = "prod_input")
#'   output <- x ^ 2
#'   dbc::assert_prod_output_number(x)
#'   return(output)
#' }
#' # function exposed to user
#' my_fun <- function(x) {
#'   my_fun_input_assertions(x, assertion_type = "user_input")
#'   my_fun_(x = x)
#' }
#' ```
#'
#' `dbc` contains a plethora of generated functions to reduce
#' repetition, improve readability, and make writing checks easier (they are
#' easy to write when you have auto-suggestions on your IDE, and start writing
#' e.g. `assert_prod_input_` and get a number of suggestions). These are
#' recommended when they can be used. When not, use `[tests_to_report]` to
#' create your own reports, and pass its output to `[report_to_assertion]` to
#' create an assertion. E.g.
#'
#' ```
#' my_fun <- function(x) {
#'   dbc::report_to_assertion(
#'     dbc::tests_to_report(
#'       expressions = "x %in% 1:5"
#'     )
#'   )
#' }
#' ```
NULL


