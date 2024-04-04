
#' @title Function Calls
#' @description
#' Retrieve upsteam function calls.
#' @name function_calls
#' @examples
#' tf1 <- function(tf1_arg = 1) {
#'   get_current_call()
#' }
#' stopifnot(
#'   identical(tf1(), quote(tf1()))
#' )
#' tf2 <- function(tf2_arg = 2) {
#'   get_parent_call()
#' }
#' tf3 <- function(tf3_arg = 3) {
#'   tf2()
#' }
#' tf4 <- function(tf4_arg = 4) {
#'   get_parent_call()
#' }
#' \donttest{
#' stopifnot(
#'   identical(tf3(), quote(tf3())),
#'   inherits(tryCatch(get_current_call(), error = function(e) e), "error"),
#'   inherits(tryCatch(get_parent_call(), error = function(e) e), "error"),
#'   inherits(tryCatch(tf4(), error = function(e) e), "error")
#' )
#' }
NULL

#' @rdname function_calls
#' @export
#' @section Functions:
#' - `get_current_call`: when called inside another function, gives the call
#'   of that function. See **Examples**.
get_current_call <- function() {
  n <- 1L
  call <- tryCatch(
    match.call(
      definition = sys.function(sys.parent(n)),
      call = sys.call(sys.parent(n)),
      envir = parent.frame(n)
    ),
    error = function(e) e
  )
  if (inherits(call, "error") || identical(call, quote(get_current_call()))) {
    stop("Could not determine crreunt call. Was get_current_call not ",
         "called in another function?")
  }
  call
}

#' @rdname function_calls
#' @export
#' @section Functions:
#' - `get_parent_call`: when called inside another function, gives the call
#'   of the function calling that one. See **Examples**.
get_parent_call <- function() {
  n <- 2L
  call <- tryCatch(
    match.call(
      definition = sys.function(sys.parent(n)),
      call = sys.call(sys.parent(n)),
      envir = parent.frame(n)
    ),
    error = function(e) e
  )
  if (inherits(call, "error") || identical(call, quote(get_parent_call()))) {
    stop("Could not determine parent call. get_parent_call can only be used ",
         "in a function called by another.")
  }
  call
}

#' @rdname function_calls
#' @export
#' @section Functions:
#' - `get_nth_call`: when called inside another function, gives the `n`'th
#'   parent call of that function.
#' @param n `[integer]` (default `1L`)
#'
#' Integer larger than zero. `1L` gives the call of the function which calls
#' `get_nth_call`.
get_nth_call <- function(n = 1L) {
  call <- tryCatch(
    match.call(
      definition = sys.function(sys.parent(n)),
      call = sys.call(sys.parent(n)),
      envir = parent.frame(n)
    ),
    error = function(e) e
  )
  if (inherits(call, "error") || identical(call, quote(get_nth_call()))) {
    stop("Could not determine nth call. get_nth_call can only be used ",
         "in a function.")
  }
  call
}


#' @title Argument Handlers
#' @description
#' Functions which handle arguments of check functions.
#' @name argument_handlers

#' @rdname argument_handlers
#' @export
#' @section Functions:
#' - `dbc::handle_arg_call` is used internally in other functions
#'   to guess `call` which is to be reported if there is a problem
#' @return
#' - `dbc::handle_arg_call`: returns an R `language` object, or `NULL` upon
#'   failure to guess the call
#' @examples
#'
#' # dbc::handle_arg_call
#' my_assert_fun <- function(x, x.nm = NULL, call = NULL) {
#'   return(dbc::handle_arg_call(call))
#' }
#' my_fun <- function(x) {
#'   return(my_assert_fun(x))
#' }
#' obs <- my_fun(1L)
#' stopifnot(
#'   deparse1(obs) == "my_fun(x = 1L)"
#' )
handle_arg_call <- function(call = NULL, env = NULL) {
  #' @template arg_call
  raise_internal_error_if_not(
    is.null(call) || is.language(call)
  )
  if (is.language(call)) {
    return(call)
  }
  #' @param env `[NULL, environment, other]` (optional, default `NULL`)
  #'
  #' - `NULL`: Use `parent.frame(1L)` or `parent.frame(2L)` --- see below.
  #' - `environment`: Use this environment.
  #'
  #' For `dbc::handle_arg_call` and `dbc::handle_arg_x_nm`, the temporary
  #' evaluation environment of the
  #' "main" function (the environment where the assertion function
  #' was called in). Hence the default for these is `parent.frame(2L)`.
  #'
  #' For others, the
  #' evaluation environment of the assertion / report / test function. Their
  #' default is `parent.frame(1L)`.
  raise_internal_error_if_not(
    is.null(env) || is.environment(env)
  )
  if (is.null(env)) {
    env <- parent.frame(2L)
  }
  call_inferrer <- quote(match.call())
  # @codedoc_comment_block news("dbc::handle_arg_call", "2024-04-04", "0.5.2")
  # `dbc::handle_arg_call` robustified in the same way as
  # `dbc::handle_arg_x_nm`, and additionally it looks at each `parent.frame(i)`
  # for `i = c(2L, 3L, 1L)` (yes, in that order).
  # @codedoc_comment_block news("dbc::handle_arg_call", "2024-04-04", "0.5.2")
  main_eval_env_candidate_set <- list(
    env,
    parent_frame_of_env(env),
    child_frame_of_env(env),
    parent.frame(2L), # yes, 2-3-1 order
    parent.frame(3L),
    parent.frame(1L)
  )
  is_bad_call <- TRUE
  for (i in seq_along(main_eval_env_candidate_set)) {
    main_eval_env_candidate <- main_eval_env_candidate_set[[i]]
    if (
      identical(main_eval_env_candidate, globalenv()) ||
        identical(main_eval_env_candidate, emptyenv)
    ) {
      next()
    }
    call <- tryCatch(
      eval(call_inferrer, envir = main_eval_env_candidate),
      error = function(e) {
        return(e)
      }
    )
    is_bad_call <- inherits(call, "error") || identical(call, call_inferrer)
    if (!is_bad_call) {
      break()
    }
  }
  if (is_bad_call) {
    # @codedoc_comment_block news("dbc::handle_arg_call", "2024-04-04", "0.5.2")
    # `dbc::handle_arg_call` returns the call from `sys.calls()` that has the
    # corresponding environment in `sys.frames()` as `env` if call cannot be
    # otherwise determined. If even that fails, return
    # `quote(could_not_determine_call)`.
    # @codedoc_comment_block news("dbc::handle_arg_call", "2024-04-04", "0.5.2")
    call <- sys.calls()[[which(vapply(
      sys.frames(),
      identical,
      logical(1L),
      y = env
    ))]]
    if (!is.language(call)) {
      call <- quote(could_not_determine_call)
      warning("Could not infer in which call an assertion was used, ",
              sprintf(
                "`%s` will be reported as the call in ",
                deparse1(call)
              ),
              "the error message, if emitted")
    }
  }
  return(call)
}

#' @rdname argument_handlers
#' @template arg_x_nm
#' @export
#' @param arg_nm `[character]` (default `"x"`)
#'
#' Actual name of argument, which must exist in `env`.
#' @section Functions:
#' - `dbc::handle_arg_x_nm` is used internally in other functions
#'   to guess the name of the object passed to argument `x` when it is not
#'   supplied explicitly. It uses object `x` in its caller environment
#'   to guess `x_nm` if it is `NULL`.
#' @return
#' - `dbc::handle_arg_x_nm`: always returns a character vector of length 1
#' @examples
#'

#' # dbc::handle_arg_x_nm
#' my_assert_fun <- function(x, x_nm = NULL, y, y_nm = NULL) {
#'   x_nm <- dbc::handle_arg_x_nm(x_nm)
#'   y_nm <- dbc::handle_arg_x_nm(y_nm, arg_nm = "y")
#'   return(mget(c("x_nm", "y_nm")))
#' }
#' my_fun <- function(x, y) {
#'   my_assert_fun(x = x, y = y)
#' }
#' obj_for_x <- 1
#' obj_for_y <- 2
#' stopifnot(
#'   identical(
#'     my_fun(x = obj_for_x, y = obj_for_y),
#'     list(x_nm = "obj_for_x", y_nm = "obj_for_y")
#'   ),
#'   identical(
#'     my_assert_fun(x = 11, y = 22),
#'     list(x_nm = "11", y_nm = "22")
#'   )
#' )
handle_arg_x_nm <- function(x_nm, env = NULL, arg_nm = "x") {
  raise_internal_error_if_not(
    (is.character(x_nm) && length(x_nm) == 1L && !is.na(x_nm)) || is.null(x_nm),
    is.environment(env) || is.null(env)
  )
  if (is.null(env)) {
    env <- parent.frame(2L)
  }
  if (is.null(x_nm)) {
    expr <- substitute(substitute(ARG_NM, env = main_eval_env_candidate),
                       list(ARG_NM = parse(text = arg_nm)[[1]]))
    # expr is now e.g. `substitute(x, env = main_eval_env)`
    # and evaluating it gives e.g. `"my_value"` if `env` is the evaluation env
    # of the "main" function which was called via `main_fun(x = "my_value")`
    # @codedoc_comment_block news("dbc::handle_arg_x_nm", "2024-04-04", "0.5.2")
    # `dbc::handle_arg_x_nm` now more robust as it also looks at the
    # environments surrounding `env` (the one preceding and one proceeding it
    # in the list of environments gotten by looping through `parent.frame(i)`).
    # @codedoc_comment_block news("dbc::handle_arg_x_nm", "2024-04-04", "0.5.2")
    main_eval_env_candidate_set <- list(
      env,
      child_frame_of_env(env),
      parent_frame_of_env(env)
    )
    x_nm <- arg_nm
    for (i  in seq_along(main_eval_env_candidate_set)) {
      main_eval_env_candidate <- main_eval_env_candidate_set[[i]]
      if (!arg_nm %in% ls(main_eval_env_candidate)) {
        next()
      }
      x_nm <- deparse1(eval(expr))
      if (x_nm != arg_nm) {
        break()
      }
    }
  }
  # @codedoc_comment_block news("dbc::handle_arg_x_nm", "2024-01-22", "0.5.0")
  # `dbc::handle_arg_x_nm` now redacts `x_nm` if it is longer than 50
  # characters.
  # @codedoc_comment_block news("dbc::handle_arg_x_nm", "2024-01-22", "0.5.0")
  nc <- nchar(x_nm)
  if (nc > 50) {
    head <- substr(x_nm, 1, 22)
    tail <- substr(x_nm, nc - 22, nc)
    x_nm <- paste0(head, "[...]", tail)
  }
  x_nm
}

parent_frame_stack <- function(n = 1L) {
  pfs <- list()
  pf <- NULL
  while (!identical(pf, globalenv())) {
    n <- n + 1L
    pf <- parent.frame(n)
    pfs[[n - 1L]] <- pf
  }
  return(pfs)
}

relative_frame_of_env <- function(env, n) {
  raise_internal_error_if_not(
    is.environment(env)
  )
  pfs <- parent_frame_stack(1L)
  env_idx <- which(vapply(pfs, identical, logical(1L), y = env))
  if (length(env_idx) != 1) {
    stop("Internal error: could not determine parent frame from the context ",
         "of `env`. If you see this message, contact the package maintainer ",
         utils::maintainer(pkg = "dbc"))
  }
  if (env_idx + n < 1 || env_idx + n > length(pfs)) {
    return(emptyenv())
  }
  pfs[[env_idx + n]]
}
parent_frame_of_env <- function(env) {
  relative_frame_of_env(env, 1L)
}
child_frame_of_env <- function(env) {
  relative_frame_of_env(env, -1L)
}

#' @rdname argument_handlers
#' @export
#' @examples
#' tf <- function(
#'   x,
#'   y,
#'   x_nm = NULL,
#'   y_nm = NULL,
#'   call = NULL,
#'   assertion_type = NULL
#' ) {
#'   dbc::handle_args_inplace()
#'   return(mget(ls()))
#' }
#' obs <- tf(1, 2)
#' stopifnot(
#'   obs[["x_nm"]] == "1",
#'   obs[["y_nm"]] == "2",
#'   grepl("^tf", deparse1(obs[["call"]])),
#'   obs[["assertion_type"]] == dbc::assertion_type_default()
#' )
#' @section Functions:
#' - `dbc::handle_args_inplace` calls `dbc::handle_arg_x_nm`,
#'   `dbc::handle_arg_call`, and `dbc::handle_arg_assertion_type` in its
#'   calling env. It also handles `x` and `env`: It checks that `x` is not
#'   missing and uses the calling env of the function that called
#'   `dbc::handle_args_inplace` if `env` is `NULL`.
#' @return
#' - `dbc::handle_args_inplace`: always `NULL` invisibly.
handle_args_inplace <- function() {
  # @codedoc_comment_block news("dbc::handle_args_inplace", "2024-01-15", "0.5.0")
  # New exported function `dbc::handle_args_inplace`.
  # @codedoc_comment_block news("dbc::handle_args_inplace", "2024-01-15", "0.5.0")
  # @codedoc_comment_block news("dbc::handle_args_inplace", "2024-01-15", "0.5.1")
  # `dbc::handle_args_inplace` now also handles `y_nm`.
  # @codedoc_comment_block news("dbc::handle_args_inplace", "2024-01-15", "0.5.1")
  parent_env <- parent.frame(1L)
  if ("x_nm" %in% ls(parent_env)) {
    parent_env[["x_nm"]] <- dbc::handle_arg_x_nm(
      parent_env[["x_nm"]],
      env = parent_env
    )
  }
  if ("y_nm" %in% ls(parent_env)) {
    parent_env[["y_nm"]] <- dbc::handle_arg_x_nm(
      x_nm = parent_env[["y_nm"]],
      env = parent_env,
      arg_nm = "y"
    )
  }
  if ("call" %in% ls(parent_env)) {
    parent_env[["call"]] <- dbc::handle_arg_call(
      call = parent_env[["call"]],
      env = parent.frame(2L)
    )
  }
  if ("assertion_type" %in% ls(parent_env)) {
    parent_env[["assertion_type"]] <- dbc::handle_arg_assertion_type(
      parent_env[["assertion_type"]]
    )
  }
  if ("env" %in% ls(parent_env) && is.null(parent_env[["env"]])) {
    parent_env[["env"]] <- parent.frame(2L)
  }
  eval(quote({
    if ("x" %in% ls() && missing(x)) {
      stop(simpleError(
        message = paste0(
          "Argument `", x_nm, "` was missing --- it has no default so ",
          "some value must be supplied!"
        ),
        call = call
      ))
    }
  }), envir = parent.frame(1L))
  return(invisible(NULL))
}

raise_internal_error_if_not <- function(...) {
  this_call <- match.call(expand.dots = TRUE)
  ddd_exprs <- match.call(expand.dots = FALSE)[["..."]]
  for (i in seq_along(ddd_exprs)) {
    error_msg <- NULL
    result <- eval(ddd_exprs[[i]], parent.frame(1L))
    if (!is.logical(result)) {
      error_msg <- paste0(
        "internal error: test ", deparse(ddd_exprs[[i]]),
        " did not evaluate to logical values; ",
        "result had class(es) ", deparse(class(result))
      )
    } else if (!all(result %in% TRUE)) {
      error_msg <- paste0(
        "internal error: not all were TRUE: ", deparse(ddd_exprs[[i]])
      )
    }
    if (!is.null(error_msg)) {
      stop(simpleError(error_msg, this_call))
    }
  }
  invisible(NULL)
}






call_with_arg_list <- function(
  fun,
  arg_list,
  envir = parent.frame(1L)
) {
  stopifnot(
    inherits(arg_list, "list"),
    is.environment(envir)
  )
  match.fun(fun)
  UseMethod("call_with_arg_list")
}

#' @export
call_with_arg_list.function <- function(
  fun,
  arg_list,
  envir = parent.frame(1L)
) {
  call_env <- new.env(parent = envir)
  call <- quote(fun())
  for (arg_nm in names(arg_list)) {
    tmp_arg_nm <- paste0("._", arg_nm)
    call[[arg_nm]] <- parse(text = tmp_arg_nm)[[1L]]
    call_env[[tmp_arg_nm]] <- arg_list[[arg_nm]]
  }
  call_env[["fun"]] <- fun
  eval(expr = call, envir = call_env)
}

#' @export
call_with_arg_list.character <- function(
  fun,
  arg_list,
  envir = parent.frame(1L)
) {
  call_env <- new.env(parent = envir)
  call <- parse(text = paste(fun, "()"))[[1L]]
  for (arg_nm in names(arg_list)) {
    tmp_arg_nm <- paste0("._", arg_nm)
    call[[arg_nm]] <- parse(text = tmp_arg_nm)[[1L]]
    call_env[[tmp_arg_nm]] <- arg_list[[arg_nm]]
  }
  call_env[[fun]] <- match.fun(fun)
  eval(expr = call, envir = call_env)
}



settings_env <- new.env(parent = emptyenv())
settings_env[["in_dev_mode"]] <- FALSE
#' @title Development Mode
#' @description
#' Set and get development mode for **dbc** functions.
#' @param value `[logical]` (mandatory, no default)
#'
#' if set to `TRUE`, "dev" assertions will be evaluated; else they won't be
#' @name dev_mode
NULL

#' @export
#' @rdname dev_mode
set_dev_mode <- function(value) {
  stopifnot(
    length(value) == 1L,
    identical(value, TRUE) || identical(value, FALSE)
  )
  settings_env[["in_dev_mode"]] <- value
}

#' @export
#' @rdname dev_mode
get_dev_mode <- function() {
  identical(settings_env[["in_dev_mode"]], TRUE)
}





.__ERROR_DATA_ENV <- new.env(parent = emptyenv())
.__ERROR_DATA_ENV[["data"]] <- list()

get_error_dataset <- function() {
  .__ERROR_DATA_ENV[["data"]]
}
add_error_data <- function(data) {
  raise_internal_error_if_not(
    inherits(data, "list"),
    c("sys.calls", "call", "msg") %in% names(data)
  )
  .__ERROR_DATA_ENV[["data"]] <- c(list(data), .__ERROR_DATA_ENV[["data"]])
  return(invisible(NULL))
}
#' @title Error Data
#' @description Retrieve data about previous errors.
#' @name error_data
#' @return
#' Both `get_error_data` and `get_newest_error_data` return a list containing
#' elements
#'
#' - `msg`: `character`; error message
#' - `call`: `call`; call where error was raised
#' - `sys.calls`: output of `[base::sys.calls]` in the context of where the
#'   error was generated

#' @param n `[integer]` (optional, default `1L`)
#'
#' position of error in list of errors in chronological order recorded during
#' this R session; `n = 1L` is the position of the newest error
#' @rdname error_data
#' @export
get_error_data <- function(n = 1L) {
  raise_internal_error_if_not(
    length(n) == 1L,
    n %% 1L == 0L,
    n > 0L
  )
  ds <- get_error_dataset()
  if (length(ds) == 0L) {
    stop("No errors recorded, cannot return anything")
  } else if (n > length(ds)) {
    stop("n = ", n, " is too large: only ", length(ds), " errors have been ",
         "recorded")
  }
  ds[[n]]
}

#' @rdname error_data
#' @export
get_newest_error_data <- function() {
  get_error_data(n = 1L)
}



list_union <- function(x) {
  stopifnot(inherits(x, "list"))
  out <- NULL
  fun_env <- environment()
  invisible(lapply(seq_along(x), function(i) {
    fun_env[["out"]] <- union(out, x[[i]])
    NULL
  }))
  return(out)
}




get_report_df_template <- function() {
  report_df_template #internal dataset
}




get_function <- function(x, env = environment(get_function)) {
  if (is.character(x)) {
    fun <- tryCatch(
      get(x = x, envir = env, mode = "function"),
      error = function(e) e
    )
    if (inherits(fun, "error")) {
      fun <- tryCatch(
        eval(parse(text = x)[[1]], envir = env),
        error = function(e) e
      )
    }
    if (inherits(fun, "error")) {
      stop("Internal error: Cannot find function based on string ", deparse(x),
           "; if you can see this, complain to the maintainer of the command ",
           "you just used")
    }
  } else if (!is.function(x)) {
    stop("Internal error: neither string nor function passed to get_function; ",
         "if you can see this, complain to the maintainer of the command ",
         "you just used")
  } else {
    fun <- x
  }
  return(fun)
}

aggregate_report_df <- function(x, pass) {
  raise_internal_error_if_not(pass %in% c("any", "all"))
  aggregated <- x[1L, ]
  aggregated[, names(aggregated)] <- lapply(names(x), function(col_nm) {
    col <- x[[col_nm]]
    switch(
      col_nm,
      test = {
        if (pass == "any") {
          out <- paste0("(", paste0(col, collapse = ") | ("), ")")
        } else {
          out <- paste0(col, collapse = " & ")
        }
        out
      },
      call = list(col[[1]]),
      n_fail = max(ifelse(is.na(col), 0L, col)),
      wh_fail = {
        wh_fail <- sort(unique(unlist(col)))
        wh_fail <- setdiff(wh_fail, NA)
        if (length(wh_fail) == 0) {
          wh_fail <- NA
        }
        list(wh_fail)
      },
      pass = switch(pass, any = any(col), all = all(col)),
      {
        if (is.character(col)) {
          col <- col[!col %in% c("NA", NA)]
          if (length(col) == 0L) {
            out <- NA_character_
          } else {
            out <- paste0(col, collapse = "; ")
          }
          out
        } else if (is.numeric(col)) {
          sum(col)
        } else if (is.logical(col)) {
          all(col %in% TRUE)
        } else {
          stop("internal error --- function has programming error. ",
               "no handling for report_df column ", deparse1(col_nm),
               "has been defined. complain to the ",
               "package maintainer.")
        }
      }
    )
  })
  if (identical(aggregated[["pass"]], TRUE)) {
    aggregated[["message"]] <- NA_character_
  }
  return(aggregated)
}
