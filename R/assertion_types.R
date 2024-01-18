
#' @title Assertion Types
#' @description
#' Tools to process assertion types.
#' @name assertion_types
NULL

#' @rdname assertion_types
#' @export
#' @eval c(
#'   "@return",
#'   "`[dbc::assertion_type_default]` returns ",
#'   deparse1(dbc::assertion_type_default())
#' )
assertion_type_default <- function() {
  # @codedoc_comment_block news("dbc::assertion_type_default", "2023-04-01", "0.4.12")
  # New fun `dbc::assertion_type_default`.
  # This returns the default value for the `assertion_type` arg used by all
  # funs that have that arg.
  # @codedoc_comment_block news("dbc::assertion_type_default", "2023-04-01", "0.4.12")
  "input"
}
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
  # @codedoc_comment_block news("dbc::assertion_types", "2022-07-25", "0.4.9")
  # Added new assertion type `"none"`. This was added for convenience:
  # using `assertion_type = "none"` means that the assertion is not performed.
  # This can save computation time.
  # @codedoc_comment_block news("dbc::assertion_types", "2022-07-25", "0.4.9")
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
    "`[NULL, character]`",
    "(default `NULL`)",
    "",
    "Must be one of the following: ",
    "",
    " - `NULL`: take `dbc::assertion_type_default()`",
    assertion_type_summary_lines_in_markdown(),
    ""
  )
  lines
}

assertion_type_error_messages <- function() {
  # @codedoc_comment_block news("dbc", "2024-01-15", "0.5.0")
  # Improved some assertion error messages --- avoided term "mis-specified".
  # @codedoc_comment_block news("dbc", "2024-01-15", "0.5.0")
  out <- list(
    general = c(
      "Invalid objects --- assertions did not pass."
    ),
    input = c(
      "One or more function inputs (arguments) were invalid."
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
      "to the author or maintainer of the function you used."
    ),
    dev_input = c(
      "Internal error: one or more arguments supplied to an internally used ",
      "function did not comply with specfications tested in development ",
      "mode."
    ),
    prod_output = c(
      "Internal error: the output of an internally used function did not ",
      "comply with specifications. ",
      "Please report this error ",
      "to the author or maintainer of the command you used."
    ),
    dev_output = c(
      "Internal error: the output of an internally used function did not ",
      "comply with specifications tested in development mode."
    ),
    prod_interim = c(
      "Internal error: an interim result inside a function was not as ",
      "expected. ",
      "Please report this error ",
      "to the author or maintainer of the command you used."
    ),
    dev_interim = c(
      "Internal error: an interim results of inside a function was not as ",
      "expected when in development mode."
    ),
    none = c(
      "If you see this error message, report to the maintainer of dbc."
    )
  )
  msg_append <- c(
    " You can use dbc::get_newest_error_data() to track down the mistake. ",
    "Assertion failures: "
  )
  out <- lapply(out, c, msg_append)
  out <- lapply(out, paste0, collapse = "")
  return(out)
}

.__ASSERTION_ERROR_MESSAGES <- assertion_type_error_messages()

assertion_error_message <- function(assertion_type) {
  .__ASSERTION_ERROR_MESSAGES[[assertion_type]]
}

#' @rdname assertion_types
#' @param messages `[character]` (no default)
#' 
#' Error messages of failed assertions.
#' @template arg_call
#' @eval arg_assertion_type_docs()
#' @export
assertion_raise <- function(
  messages,
  call,
  assertion_type
) {
  # @codedoc_comment_block news("dbc::assertion_raise", "2024-01-15", "0.5.0")
  # New function `dbc::assertion_raise`. This is used every time `dbc`
  # raises an error over an assertion that did not pass.
  # @codedoc_comment_block news("dbc::assertion_raise", "2024-01-15", "0.5.0")
  message <- paste0(
    assertion_error_message(assertion_type),
    "\n",
    paste0(" - ", messages, collapse = "\n")
  )
  sys_calls <- sys.calls()
  add_error_data(
    list(
      call = call,
      msg = message,
      sys.calls = sys_calls[-length(sys_calls)]
    )
  )
  stop(simpleError(message, call))
}

#' @rdname assertion_types
#' @param expression `[call]` (no default)
#' 
#' Expression to evaluate.
#' @param fail_message `[NULL, character]` (default `NULL`)
#' 
#' - `NULL`: Use a default message.
#' - `character`: Use this message.
#' 
#' Error message of failed assertion. Passed to [interpolate] before it is
#' emitted.
#' @param env `[environment]` (default `parent.frame(1L)`, i.e. calling env)
#' 
#' Environment where `expression` will be evaluated.
#' @template arg_x_nm
#' @template arg_call
#' @eval arg_assertion_type_docs()
#' @export
assertion_eval <- function(
  expression,
  fail_message = NULL,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  env = NULL
) {
  # @codedoc_comment_block news("dbc::assertion_eval", "2024-01-16", "0.5.0")
  # New function `dbc::assertion_eval`. Assertion functions generated based on
  # expressions make use of this.
  # @codedoc_comment_block news("dbc::assertion_eval", "2024-01-16", "0.5.0")
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  stopifnot(
    is.call(expression),
    is.environment(env),
    inherits(fail_message, c("NULL", "character")),
    length(fail_message) %in% 0:1
  )
  dbc::handle_args_inplace()
  if (is.null(fail_message) || is.na(fail_message)) {
    fail_message <- sprintf("Test `%s` failed.", deparse1(expression))
  }
  result <- dbc::expression_eval(
    expression = expression,
    eval_parent_env = env
  )
  if (!identical(result[["pass"]], TRUE)) {
    interpolate_env <- result[["eval_env"]]
    result["eval_env"] <- NULL
    lapply(names(result), function(result_name) {
      interpolate_env[[result_name]] <- result[[result_name]]
    })
    interpolate_env[["x_nm"]] <- x_nm
    interpolate_env[["call"]] <- call
    parent.env(interpolate_env) <- env
    if (!identical(result[["error"]], NA_character_)) {
      fail_message <- sprintf(
        "Test `%s` with x = %s raised an error: \"%s\"",
        deparse1(expression),
        x_nm,
        result[["error"]]
      )
    } else {
      fail_message <- dbc::interpolate(fail_message, env = interpolate_env)
    }
    dbc::assertion_raise(
      messages = fail_message,
      call = call,
      assertion_type = assertion_type
    )
  }
}

#' @rdname assertion_types
#' @export
#' @eval arg_assertion_type_docs()
#' @return
#' `[dbc::handle_assertion_type]` returns `assertion_type`,
#' except `assertion_type = NULL`
#' is replaced with `assertion_type <- dbc::assertion_type_default()`.
handle_arg_assertion_type <- function(
  assertion_type
) {
  # @codedoc_comment_block news("dbc::handle_arg_assertion_type", "2023-06-27", "0.4.14")
  # New function `[dbc::handle_arg_assertion_type]`. Currently returns
  # `assertion_type` as-is, except `assertion_type = NULL`
  # is replaced with `assertion_type <- dbc::assertion_type_default()`.
  # Raises an error if `assertion_type` is not `NULL` nor one of
  # `dbc::assertion_types()`.
  # @codedoc_comment_block news("dbc::handle_arg_assertion_type", "2023-06-27", "0.4.14")
  if (is.null(assertion_type)) {
    assertion_type <- dbc::assertion_type_default()
  } else {
    if (!assertion_type %in% dbc::assertion_types()) {
      stop("assertion_type must be NULL or one of ",
           deparse1(dbc::assertion_types()))
    }
  }
  return(assertion_type)
}
