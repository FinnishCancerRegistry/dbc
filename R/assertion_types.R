
#' @title Assertion Types
#' @description
#' Tools to process assertion types.
#' @name assertion_types
NULL

#' @rdname assertion_types
#' @export
#' @eval c(
#'   "@return",
#'   "`[dbc::assertion_types]` returns vector ",
#'   deparse(dbc::assertion_types())
#' )
assertion_types <- function() {
  names(assertion_type_summaries())
}
#' @rdname assertion_types
#' @export
#' @eval c(
#'   "@return",
#'   "`[dbc::dev_assertion_types]` returns vector ",
#'   deparse(dbc::dev_assertion_types())
#' )
dev_assertion_types <- function() {
  at <- assertion_types()
  at[grepl("^dev_", at)]
}

assertion_type_summaries <- function() {
  list(
    general = c(
      "just says that assertions did not pass without information",
      "as to whose fault this was --- just that some object was not as expected"
    ),
    input = c(
      "assertion error messages direct the attention towards the ",
      "inputs (arguments) of guilty function"
    ),
    user_input = c(
      "the end-user is directed to adjust their arguments."
    ),
    prod_input = c(
      "the assertion error is considered to be an internal error,",
      "and the end-user is directed to report it; the inputs of some function",
      "were not as expected"
    ),
    dev_input = c(
      "only the developer is notified (see ",
      "`[dbc::set_dev_mode]`)"
    ),
    prod_output = c(
      "like `\"prod_input\"`, but the output of some function",
      "was not as expected"
    ),
    dev_output = c(
      "like `\"prod_output\"`, but only raised in development mode",
      "(see `[dbc::set_dev_mode]`)"
    ),
    prod_interim = c(
      "like `\"prod_input\"`, but the interim result somewhere",
      "was not as expected"
    ),
    dev_interim = c(
      "like `\"prod_interim\"`, but only raised in development mode",
      "(see `[dbc::set_dev_mode]`)"
    ),
    none = c(
      "added for convenience, this means that the assertion is not performed"
    )
  )
}
assertion_type_summary_lines_in_markdown <- function()  {
  ats <- assertion_type_summaries()
  unlist(lapply(names(ats), function(at_nm) {
    lines <- ats[[at_nm]]
    lines[1] <- paste0(" - `\"", at_nm, "\"`: ", lines[1])
    c("", lines, "")
  }))
}
arg_assertion_type_docs <- function() {
  lines <- c(
    "@param assertion_type",
    "`[character]`",
    "(default usually `\"general\"`)",
    "",
    "Must be exactly one of the following: ",
    "",
    assertion_type_summary_lines_in_markdown(),
    ""
  )
  lines
}

assertion_type_error_messages <- function() {
  msg_append <- c(
    "You can use dbc::get_newest_error_data() to inspect ",
    "in more detail where the error occurred. ",
    "These were the errors: "
  )
  out <- list(
    general = c(
      "One or more assertions failed. Object(s) were mis-specified. "
    ),
    input = c(
      "One or more function inputs (arguments) were mis-specified. "
    ),
    user_input = c(
      "Hi user! One or more arguments you supplied did not comply with ",
      "their specifications; please see the points below and adjust ",
      "your arguments."
    ),
    prod_input = c(
      "Internal error: one or more arguments supplied to an internally used ",
      "function did not comply with specfications. ",
      "Please report this error ",
      "to the author or maintainer of the command you used."
    ),
    dev_input = c(
      "Internal error: one or more arguments supplied to an internally used ",
      "function did not comply with specfications tested in development ",
      "mode. "
    ),
    prod_output = c(
      "Internal error: the output of an internally used function did not ",
      "comply with specifications. ",
      "Please report this error ",
      "to the author or maintainer of the command you used. "
    ),
    dev_output = c(
      "Internal error: the output of an internally used function did not ",
      "comply with specifications tested in development mode. ",
      "These were the errors: "
    ),
    prod_interim = c(
      "Internal error: an interim result inside a function was not as ",
      "expected. ",
      "Please report this error ",
      "to the author or maintainer of the command you used. "
    ),
    dev_interim = c(
      "Internal error: an interim results of inside a function was not as ",
      "expected when in development mode. "
    )
  )
  out <- lapply(out, c, msg_append)
  return(out)
}
