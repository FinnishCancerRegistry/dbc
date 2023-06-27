
#' @title Expressions to Reports to Assertions
#' @description
#' Collect tests into a report data.frame, raise assertion errors in failed
#' tests in report.
#' @name expressions_to_reports_to_assertions
NULL

#' @rdname expressions_to_reports_to_assertions
#' @export
#' @eval codedoc::codedoc_lines("^dbc::expressions_to_report")
#' @param fail_messages `[NULL, character]` (default `NULL`)
#'
#' - `NULL`: use `"at least one FALSE value in test: ${test}"`
#' - `character`: use these messages (one for each test); `NA_character_`
#'   values are replaced with the same message as when this argument is
#'   `NULL`
#'
#' messages (one for each test) to include in report upon failure; see section
#' **Interpolation in messages**
#' @param pass_messages `[NULL, character]` (default `NULL`)
#'
#' - `NULL`: use `"all were TRUE in test: ${test}"`
#' - `character`: use these messages (one for each test); `NA_character_`
#'   values are replaced with the same message as when this argument is
#'   `NULL`
#'
#' as `fail_messages` but for tests successes
#' @template arg_call
#'
#' @return
#' For `expressions_to_report`, a `data.frame` with columns
#'
#' - `test`: argument `tests` as-is
#' - `error`: the error message if the test resulted in an error; `NA` if it
#'   did not
#' - `pass`: `TRUE` if corresponding test evaluated to logical and all were
#'   `TRUE`, otherwise `FALSE` (i.e. any `NA` values leads to `FALSE`)
#' - `n_fail`: number of elements of result of test that were `FALSE` or `NA`;
#'   if result was not a logical vector of length greater than one, this is `NA`
#' - `wh_fail`: integer vector of positions of test result that were `FALSE`
#'   or `NA`; if result was not a logical vector of length greater than one,
#'   this is `NA`
#' - `message`: the corresponding `fail_messages` or `pass_messages`
#'   element (after interpolation) depending on `pass`
#'
#' @examples
#' # expressions_to_report
#'
#' a <- 1:5
#' b <- 1:5
#' # pass
#' expressions_to_report(
#'   expressions = "a == b"
#' )
#' # pass
#' expressions_to_report(
#'   expressions = list(quote(a == b))
#' )
#' # expressions_to_report raises error
#' out <- tryCatch(expressions_to_report(
#'   expressions = a == (b + 1)
#' ), error = function(e) e)
#' stopifnot(inherits(out, "error"))
#' # fail
#' expressions_to_report(
#'   expressions = "a == (b + 1)"
#' )
#' # expressions_to_report does not raise error, but shows it in output
#' expressions_to_report(
#'   expressions = "a == d"
#' )
#'
#' # interpolation example
#' a <- 1
#' rdf <- dbc::expressions_to_report(
#'   expressions = "a == 1",
#'   pass_messages = "a was ${a} as expected",
#'   env = environment()
#' )
#' stopifnot(
#'   rdf[["message"]] == "a was 1 as expected"
#' )
#'
#' # interpolation example
#' a <- 1:4
#' rdf <- dbc::expressions_to_report(
#'   expressions = "(a_len <- length(a)) == 5L",
#'   pass_messages = "Length of a was ${a_len} as expected",
#'   fail_messages = "Length of a was ${a_len}; 5 was expected",
#'   env = environment()
#' )
#' stopifnot(
#'   rdf[["message"]] == "Length of a was 4; 5 was expected"
#' )
expressions_to_report <- function(
  expressions,
  fail_messages = NULL,
  pass_messages = NULL,
  env = parent.frame(1L),
  call = NULL
) {
  # assertions etc -------------------------------------------------------------
  call <- dbc::handle_arg_call(call)

  # @codedoc_comment_block dbc::expressions_to_report::expressions
  # @param expressions `[character, list]` (no default)
  #
  # - `character`: vector of expressions to perform; after parsing each string
  #   the evaluated expression must return a logical vector or `NULL`,
  #   where `NULL` is interpreted as pass
  # - `list`: each element is either a character string as above or a language
  #   object (see e.g. [quote]) which is deparsed into a string
  # @codedoc_comment_block dbc::expressions_to_report::expressions
  raise_internal_error_if_not(
    inherits(expressions, c("list", "character"))
  )
  expressions <- as.list(expressions)
  expressions <- lapply(seq_along(expressions), function(i) {
    raise_internal_error_if_not(
      is.language(expressions[[i]]) || is.character(expressions[[i]])
    )
    expr <- expressions[[i]]
    if (is.character(expr)) {
      expr <- parse(text = expr)[[1L]]
    }
    return(expr)
  })
  expression_strings <- vapply(expressions, function(expr) {
    deparse1(expr)
  }, character(1L))

  raise_internal_error_if_not(
    is.null(fail_messages) || (
      is.character(fail_messages) &&
        length(fail_messages) %in% c(1L, length(expressions))
    ),

    is.null(pass_messages) || (
      is.character(pass_messages) &&
        length(pass_messages) %in% c(1L, length(expressions))
    ),

    is.environment(env)
  )

  # fail / pass messages -------------------------------------------------------
  if (is.null(fail_messages)) {
    fail_messages <- rep(NA_character_, length(expressions))
  } else if (length(fail_messages) == 1L) {
    fail_messages <- rep(fail_messages, length(expressions))
  }
  fail_messages[is.na(fail_messages)] <- paste0(
    "test failed: ", expressions[is.na(fail_messages)]
  )
  if (is.null(pass_messages)) {
    pass_messages <- rep(NA_character_, length(expressions))
  } else if (length(pass_messages) == 1L) {
    pass_messages <- rep(pass_messages, length(expressions))
  }
  pass_messages[is.na(pass_messages)] <- paste0(
    "test passed: ", expressions[is.na(pass_messages)]
  )

  # report_df ------------------------------------------------------------------
  report_df <- data.frame(
    test = expression_strings,
    error = NA_character_,
    pass = NA,
    n_fail = NA_integer_,
    wh_fail = NA_integer_,
    message = NA_character_,
    call = NA_character_,
    stringsAsFactors = FALSE
  )
  report_df[["wh_fail"]] <- rep(list(integer(0L)), nrow(report_df))
  report_df[["call"]] <- rep(list(call), nrow(report_df))

  # evaluation -----------------------------------------------------------------
  fun_env <- environment()
  lapply(seq_along(expressions), function(i) {
    expression_string <- expressions[i]
    test_expr <- parse(text = expression_string)[[1L]]

    # @codedoc_comment_block dbc::expressions_to_report::env
    # @param env `[environment]` (default `parent.frame(1L)`)
    #
    # `env` will be the parent environment of the environment where each element
    # of `expressions` is evaluated. A new, separate environment is created for
    # each separate expression (*expression evaluation environment*).
    # They all have `env` as the parent environment (*context environment*).
    # @codedoc_comment_block dbc::expressions_to_report::env
    eval_env <- new.env(parent = env)
    result <- tryCatch(
      eval(test_expr, envir = eval_env),
      error = function(e) e
    )
    pass <- FALSE
    error <- NA_character_
    n_fail <- NA_integer_
    wh_fail <- NA_integer_
    if (inherits(result, "error")) {
      error <- result[["message"]]
    } else if (is.null(result)) {
      pass <- TRUE
    } else if (is.logical(result)) {
      passing_elems <- result %in% TRUE # %in% avoids NA's. NA == TRUE -> NA
      pass <- all(passing_elems)
      if (length(result) != 1L) {
        wh_fail <- which(!passing_elems)
      }
      n_fail <- sum(!passing_elems)
    } else {
      stop(
        "test ", deparse(expression_string), " returned result of class(es) ",
        deparse(class(result)), "; logical or NULL was expected; see ",
        "help for argument 'tests'"
      )
    }
    # @codedoc_comment_block dbc::expressions_to_report::env
    #
    # `env` is also used in interpolation --- see section
    # **Message interpolation**.
    # @codedoc_comment_block dbc::expressions_to_report::env

    # @codedoc_comment_block dbc::expressions_to_report::interpolation
    # @section Message interpolation:
    #
    # Simple string interpolation is supported in `pass_messages` and
    # `fail_messages`. For the purpose of evaluating the expression substrings
    # in the messages, a new empty environment (*interpolation environment*)
    # is created. This contains any objects created when the `expressions` element
    # was evaluated. It's parent environment (*report environment*) contains
    # objects
    # `test`, `error`, `pass`, `n_fail`, and `wh_fail`, respectively. These are
    # the data with which the report data.frame is created, ultimately.
    # The parent environment of the *report environment* is the same as that env
    # where the `expressions` element was evaluated.
    #
    # *context environment*
    #  -> *expression evaluation environment*
    #
    # *context environment*
    #  -> *report environment*
    #    -> *interpolation environment*
    #
    # These environment tricks allows one to use objects in the
    # *context environment* in both expression evaluation and in interpolation.
    # The objects in the *report environment* can be used in interpolation.
    #
    # @codedoc_insert_comment_block dbc:::interpolate
    # @codedoc_comment_block dbc::expressions_to_report::interpolation
    fun_env[["report_df"]][["error"]][i] <- error
    fun_env[["report_df"]][["pass"]][i]  <- pass
    fun_env[["report_df"]][["n_fail"]][i] <- n_fail
    fun_env[["report_df"]][["wh_fail"]][[i]] <- wh_fail

    report_environment <- as.environment(mget(
      c("error", "pass", "n_fail")
    ))
    report_environment[["wh_fail"]] <- fun_env[["report_df"]][["wh_fail"]][[1L]]
    interpolation_environment <- eval_env
    parent.env(interpolation_environment) <- report_environment
    parent.env(report_environment) <- env
    msg <- pass_messages[i]
    if (!fun_env[["report_df"]][["pass"]][i]) {
      msg <- fail_messages[i]
    }
    fun_env[["report_df"]][["message"]][i] <- interpolate(
      msg, env = interpolation_environment
    )
    NULL
  })

  return(report_df)
}


#' @rdname expressions_to_reports_to_assertions
#' @export
#' @param report_df `[data.frame]` (no default)
#'
#' a report `data.frame` as returned by `expressions_to_report`
#' @eval arg_assertion_type_docs()
#' @param raise_error_call `[NULL, language]` (default `NULL`)
#'
#' the call to display in the error call; passed to arg `call` of
#' [base::simpleError].
#'
#' - `NULL`: use the parent call; when this function is called in another
#'   function (as it is intended to be used), the function call of the
#'   surrounding function is used; see **Examples**
#' - `language`: this call is used as-is.
#'
#'
#' @examples
#' # report to assertion
#'
#' # pass
#' report_df <- expressions_to_report("1 == 1")
#' report_to_assertion(report_df)
#'
#' # fail
#' report_df <- expressions_to_report("1 == 2")
#' tryCatch(
#'   report_to_assertion(report_df),
#'   error = function(e) e
#' )
#'
#' # 2 passes, 2 failures
#' report_df <- expressions_to_report(c("1 == 2", "1 == 1", "2 == 2", "2 == 1"))
#' tryCatch(
#'   report_to_assertion(report_df),
#'   error = function(e) e
#' )
#'
#' my_fun <- function(my_arg) {
#'   report_to_assertion(expressions_to_report("is.character(my_arg)"))
#' }
#' tryCatch(
#'   my_fun(my_arg = 1L),
#'   error = function(e) e
#' )
report_to_assertion <- function(
  report_df,
  assertion_type = NULL,
  raise_error_call = NULL
) {
  raise_internal_error_if_not(
    is.data.frame(report_df),
    c("pass", "message", "error", "call") %in% names(report_df),

    length(assertion_type) %in% 0:1,
    assertion_type %in% c(as.list(assertion_types()), list(NULL)),

    is.null(raise_error_call) || is.language(raise_error_call)
  )
  raise_error_call <- dbc::handle_arg_call(raise_error_call)

  if (is.null(assertion_type)) {
    assertion_type <- dbc::assertion_type_default()
  }
  if (assertion_type %in% dev_assertion_types() && get_dev_mode() == FALSE) {
    return(invisible(NULL))
  }
  report_df[, names(report_df)] <- lapply(report_df, function(col) {
    if (is.factor(col)) {
      col <- as.character(col)
    }
    col
  })
  wh_nonpass <- which(!report_df[["pass"]] %in% TRUE)
  if (length(wh_nonpass) > 0L) {
    msgs <- vapply(
      wh_nonpass,
      function(test_no) {
        suffix <- paste0("failed: ", report_df[["message"]][test_no])
        error_msg <- report_df[["error"]][test_no]
        if (!is.na(error_msg)) {
          suffix <- paste0("encountered an ERROR: ", error_msg)
        }
        expression_string <- paste0(deparse(
          report_df[["test"]][test_no]
        ), collapse = "")
        paste0("test ", expression_string, " ", suffix)
      },
      character(1L)
    )
    call_strings <- vapply(wh_nonpass, function(test_no) {
      call <- report_df[["call"]][[test_no]]
      if (is.null(call)) {
        call_string <- "in unknown call, "
      } else {
        call_string <- paste0(
          "in call ", paste0(deparse(call), collapse = ""), ", "
        )
      }
      gsub("\\s+", " ", call_string)
    }, character(1L))
    msgs <- paste0(call_strings, msgs)

    msg_start <- paste0(assertion_type_error_messages()[[assertion_type]],
                        collapse = "")
    msg <- paste0(
      c(msg_start, paste0(" - ", msgs)),
      collapse = "\n"
    )
    sys_calls <- sys.calls()
    add_error_data(
      list(
        call = raise_error_call,
        msg = msg,
        sys.calls = sys_calls[-length(sys_calls)]
      )
    )
    stop(simpleError(msg, raise_error_call))
  }

  return(invisible(NULL))
}
