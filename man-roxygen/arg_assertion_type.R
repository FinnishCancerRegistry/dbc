
# this file generated automatically by dbc:::arg_assertion_type_docs
#' @param assertion_type
#' `[character]`
#' (default usually `"general"`)
#' 
#' Must be exactly one of the following: 
#' 
#' 
#'  - `"general"`: just says that assertions did not pass without information
#' as to whose fault this was --- just that some object was not as expected
#' 
#' 
#'  - `"input"`: assertion error messages direct the attention towards the 
#' inputs (arguments) of guilty function
#' 
#' 
#'  - `"user_input"`: the end-user is directed to adjust their arguments.
#' 
#' 
#'  - `"prod_input"`: the assertion error is considered to be an internal error,
#' and the end-user is directed to report it; the inputs of some function
#' were not as expected
#' 
#' 
#'  - `"dev_input"`: only the developer is notified (see 
#' `[dbc::set_dev_mode]`)
#' 
#' 
#'  - `"prod_output"`: like `"prod_input"`, but the output of some function
#' was not as expected
#' 
#' 
#'  - `"dev_output"`: like `"prod_output"`, but only raised in development mode
#' (see `[dbc::set_dev_mode]`)
#' 
#' 
#'  - `"prod_interim"`: like `"prod_input"`, but the interim result somewhere
#' was not as expected
#' 
#' 
#'  - `"dev_interim"`: like `"prod_interim"`, but only raised in development mode
#' (see `[dbc::set_dev_mode]`)
#' 

