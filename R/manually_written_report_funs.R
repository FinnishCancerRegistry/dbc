




#' @rdname assertions
#' @export
#' @return
#' - `report_has_invalid_observations`: returns a report `data.frame`.
#' @param x_nm `[character]` (optional, default `NULL`)
#'
#' see e.g. [report_is_atom]
#' @param expressions `[character]` (mandatory, no default)
#'
#' passed to [expressions_to_report] as-is
#' @param fail_messages `[character]` (optional, default `NULL`)
#'
#' passed to [expressions_to_report] as-is
#' @param pass_messages `[character]` (optional, default `NULL`)
#'
#' passed to [expressions_to_report] as-is
#' @param col_nms `[character]` (optional, default `names(x)`)
#'
#' you may limit to such expressions which only use these column names; see
#' `col_nm_set_list`
#' @param col_nm_set_list `[NULL, list]` (optional, default `NULL`)
#'
#' - `NULL`: all expressions are used
#' - `list`: must be of length `length(expressions)`, where each element is a
#'   character vector of column names; the corresponding test is only run
#'   if `all(col_nm_set_list[[i]] %in% col_nms)` for all
#'   `i in seq_along(expressions)`. this allows for easy limiting of expressions to run
#'   in when not all of them need to be run.
#' @param by `[character]` (no default)
#' 
#' Column names as strings.
#' @details
#' - `report_has_invalid_observations` calls [expressions_to_report] where the
#'   dataset `x` becomes the environment where the expressions are evaluated
#'   (essentially `env = as.environment(x)`)
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   expressions = c("a == b", "a == d", "a != b")
#' )
#'
#' # running expressions only for some columns
#' df <- data.frame(a = 1:5, b = 1:5, c = 1:5)
#' report_df <- report_has_only_valid_observations(
#'   x = df,
#'   expressions = c("a == b", "a == c", "a != b"),
#'   col_nms = c("a", "b"),
#'   col_nm_set_list = list(c("a", "b"), c("a", "c"), c("a", "b"))
#' )
report_has_only_valid_observations <- function(
  x,
  x_nm = NULL,
  expressions,
  fail_messages = NULL,
  pass_messages = NULL,
  col_nms = names(x),
  col_nm_set_list = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  assert_is_data.frame(x = x, x_nm = x_nm)
  assert_is_character_nonNA_vector(expressions)
  assert_is_one_of(
    x = col_nm_set_list,
    funs = c("report_is_NULL", "report_is_list")
  )
  parent_env <- parent.frame(1L)
  dataset_env <- as.environment(x)
  parent.env(dataset_env) <- parent_env
  call <- dbc::handle_arg_call(call)

  which_tests_to_run <- seq_along(expressions)
  if (inherits(col_nm_set_list, "list")) {
    lapply(col_nm_set_list, assert_is_character_nonNA_vector)
    assert_is_character_nonNA_vector(col_nms)
    assert_vector_elems_are_in_set(col_nms, set = names(x))
    assert_is_of_length(col_nm_set_list, expected_length = length(expressions))

    which_tests_to_run <- which(vapply(
      seq_along(expressions),
      function(test_no) {
        test_col_nm_set <- col_nm_set_list[[test_no]]
        all(test_col_nm_set %in% col_nms)
      },
      logical(1L)
    ))
    if (length(which_tests_to_run) == 0L) {
      report_df <- expressions_to_report(
        expressions = "1 == 1",
        fail_messages = fail_messages,
        pass_messages = pass_messages,
        env = dataset_env
      )
      return(report_df[0L, ])
    }
  }

  if (length(fail_messages) == 1L) {
    fail_messages <- rep(fail_messages, length(which_tests_to_run))
  }
  if (length(pass_messages) == 1L) {
    pass_messages <- rep(pass_messages, length(which_tests_to_run))
  }

  expressions_to_report(
    expressions = expressions[which_tests_to_run],
    fail_messages = fail_messages,
    pass_messages = pass_messages,
    env = dataset_env
  )
}



#' @rdname assertions
#' @param template `[any R object]` (no default)
#'
#' `x` must look similar to this object. See `compare` for what information
#' is compared between the two.
#' @param compare `[NULL, character]` (default `NULL`)
#'
#' If `NULL`, use all options.
#'
#' These are the options:
#' - `"names"`: Compare names of the objects. `x` must have all names that
#'    `template` has. It may have additional names.
#' - `"classes"`: Compare classes of the objects. `x` must have all the classes
#'   that template has. It may have additional classes.
#' - `"lengths"`: Compare lengths of objects. `x` must have the same length
#'   that `template` has.
#'
#' If `template` and `x` return `TRUE` for `is.list`, comparisons are performed
#' recursively on all elements and their elements, and so on, until a list
#' object is no longer encountered. See **Examples**.
#' @examples
#'
#' # dbc::report_is_like_template
#'
#' x_1 <- data.frame(a = 1:3, b = c("a", "b", "c"), c = factor(1:3))
#' template_1 <- data.frame(a = integer(0L), b = character(0L),
#'                          c = numeric(0L))
#' report_1 <- dbc::report_is_like_template(x = x_1, template = template_1,
#'                                          compare = c("names", "classes"))
#' stopifnot(sum(report_1$pass == FALSE) == 1)
#'
#' x_2 <- list(a = list(a1 = 1:3, a2 = 1:3 + 0.5), b = 1L)
#' template_2 <- list(a = list(a1 = integer(0L), a2 = numeric(0L)),
#'                    b = list(b1 = character(0L)))
#' report_2 <- dbc::report_is_like_template(x = x_2, template = template_2,
#'                                          compare = c("names", "classes"))
#' stopifnot(sum(report_2$pass == FALSE) == 3)
#'
#' x_3 <- 2:4
#' template_3 <- 1:3
#' report_3 <- dbc::report_is_like_template(
#'   x = x_3, template = template_3,
#'   compare = c("names", "classes", "lengths")
#' )
#' stopifnot(all(report_3$pass))
#'
#' @export
report_is_like_template <- function(
  x,
  x_nm = NULL,
  call = NULL,
  template,
  compare = NULL
) {
  # @codedoc_comment_block news("dbc::report_is_like_template", "2022-07-20", "0.4.7")
  # New fun `dbc::report_is_like_template`. Generated correspoding assertion
  # funs. Compare an object's names, class, and length to a template. Recursive
  # for lists.
  # @codedoc_comment_block news("dbc::report_is_like_template", "2022-07-20", "0.4.7")
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::assert_prod_input_is_one_of(
    x = compare,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_vector)
  )
  compare_options <- c("names", "classes", "lengths")
  if (is.null(compare)) {
    compare <- compare_options
  } else {
    dbc::assert_prod_input_vector_elems_are_in_set(
      compare, set = compare_options
    )
  }

  df <- NULL
  if ("names" %in% compare && !is.null(names(template))) {
    df_nm <- dbc::report_has_names(
      x = x,
      x_nm = x_nm,
      call = call,
      required_names = names(template)
    )
    df <- rbind(df, df_nm)
  }
  if ("classes" %in% compare) {
    df_class <- dbc::expressions_to_report(
      expressions = "class(template) %in% class(x)",
      fail_messages = paste0(
        "expected ", x_nm, " to have all of the following classes: ",
        deparse(class(template)), "; instead it had this set of classes: ",
        deparse(class(x))
      ),
      call = call,
      env = environment()
    )
    df <- rbind(df, df_class)
  }
  if ("lengths" %in% compare) {
    df_length <- dbc::report_has_length(
      x = x,
      x_nm = x_nm,
      call = call,
      expected_length = length(template)
    )
    df <- rbind(df, df_length)
  }
  if (is.list(template) && identical(class(template), class(x))) {
    df_recursive <- do.call(
      rbind,
      lapply(
        seq_along(template),
        function(i) {
          ref <- i
          ref_nm <- i
          if (!is.null(names(template))) {
            ref <- names(template)[i]
            ref_nm <- paste0("\"", ref, "\"")
          }
          sub_df <- report_is_like_template(
            x = x[[ref]],
            x_nm = paste0(x_nm, "[[", ref_nm, "]]"),
            call = call,
            template = template[[ref]],
            compare = compare
          )
          return(sub_df)
        }
      ),
      quote = TRUE
    )
    df <- rbind(df, df_recursive)
  }

  return(df)
}








#' @rdname assertions
#' @param y `[any R object]` (no default)
#'
#' Compare `x` to this.
#' @param y_nm `[NULL, character]` (no default)
#'
#' As `x_nm`, but for `y`.
#' @examples
#'
#' # dbc::report_is_identical
#' a <- 1L
#' b <- 1.0
#' c <- 1L
#' stopifnot(
#'   !dbc::report_is_identical(x = a, y = b)[["pass"]],
#'    dbc::report_is_identical(x = a, y = c)[["pass"]]
#' )
#'
#' @export
report_is_identical <- function(
    x,
    x_nm = NULL,
    call = NULL,
    y,
    y_nm = NULL
) {
  # @codedoc_comment_block news("dbc::report_is_identical", "2022-08-18", "0.4.10")
  # New fun `dbc::report_is_identical`. Generated correspoding assertion
  # funs.
  # @codedoc_comment_block news("dbc::report_is_identical", "2022-08-18", "0.4.10")
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (is.null(y_nm)) {
    y_nm <- deparse1(substitute(y))
  }
  eval_env <- environment()
  df <- dbc::expressions_to_report(
    expressions = "identical(x, y)",
    fail_messages = paste0(
      x_nm, " was not identical to ", y_nm
    ),
    env = eval_env,
    call = call
  )
  return(df)
}



#' @rdname assertions
#' @param y `[any R object]` (no default)
#'
#' Compare `x` to this.
#' @param y_nm `[NULL, character]` (no default)
#'
#' As `x_nm`, but for `y`.
#' @param all_equal_arg_list `[NULL, list]` (default `NULL`)
#'
#' - `NULL`: No effect, call `[all.equal]` with defaults.
#' - `list`: Pass these additional arguments to `[all.equal]`.
#' @examples
#'
#' # dbc::report_is_equal
#' a <- 1L
#' b <- 1.0
#' c <- 2L
#' stopifnot(
#'   dbc::report_is_all_equal(x = a, y = b)[["pass"]],
#'  !dbc::report_is_all_equal(x = a, y = c)[["pass"]]
#' )
#' df1 <- data.frame(a1 = 1:5, b1 = 5:1)
#' df2 <- data.frame(a2 = 1:5, b2 = 5:1)
#' rdf1 <- dbc::report_is_all_equal(
#'   x = df1, y = df2
#' )
#' rdf2 <- dbc::report_is_all_equal(
#'   x = df1, y = df2, all_equal_arg_list = list(check.attributes = FALSE)
#' )
#' stopifnot(
#'  !rdf1[["pass"]],
#'   rdf2[["pass"]]
#' )
#'
#' @export
report_is_all_equal <- function(
    x,
    x_nm = NULL,
    call = NULL,
    y,
    y_nm = NULL,
    all_equal_arg_list = NULL
) {
  # @codedoc_comment_block news("dbc::report_is_all_equal", "2022-08-18", "0.4.10")
  # New fun `dbc::report_is_all_equal`. Generated correspoding assertion
  # funs.
  # @codedoc_comment_block news("dbc::report_is_all_equal", "2022-08-18", "0.4.10")
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (is.null(y_nm)) {
    y_nm <- deparse1(substitute(y))
  }
  dbc::assert_is_one_of(
    all_equal_arg_list,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_list),
    assertion_type = "prod_input"
  )
  eval_env <- environment()
  all_equal_arg_list <- as.list(all_equal_arg_list)
  if (is.null(names(all_equal_arg_list))) {
    names(all_equal_arg_list) <- rep("", length(all_equal_arg_list))
  }
  arg_symbol_list <- c(
    list(target = quote(x), current = quote(y)),
    structure(lapply(seq_along(all_equal_arg_list), function(i) {
      obj <- all_equal_arg_list[[i]]
      is_small <- is.vector(obj) && !is.list(obj) && length(obj) <= 5
      if (is_small) {
        return(all_equal_arg_list[[i]])
      }
      nm_i <- names(all_equal_arg_list)[i]
      if (nm_i == "") {
        substitute(all_equal_arg_list[[i]], list(i = i))
      } else {
        substitute(all_equal_arg_list[[nm]], list(nm = nm_i))
      }
    }), names = names(all_equal_arg_list))
  )
  expr <- do.call(
    what = "call",
    args = c(list(name = "all.equal"), arg_symbol_list),
    quote = TRUE
  )
  expr <- substitute(isTRUE(eq <- call), list(call = expr))
  df <- dbc::expressions_to_report(
    expressions = list(expr),
    fail_messages = paste0(
      x_nm, " was not equal to ", y_nm, "; ",
      "message from all.equal: ",
      "${deparse(eq)}"
    ),
    env = eval_env,
    call = call
  )
  return(df)
}

#' @rdname assertions
#' @export
#' @param funs `[character, list]` (mandatory, no default)
#'
#' report functions that return a report (data.frame);
#' - `character`: names of functions that can be found by `[match.fun]`
#' - `list`: list of functions
#' @examples
#'
#' # dbc::report_is_one_of
#' my_var <- 1:3
#' rdf <- dbc::report_is_one_of(
#'   my_var,
#'   funs = list(dbc::report_is_NULL, dbc::report_is_character_nonNA_vector)
#' )
#' stopifnot(
#'   nrow(rdf) == 1L,
#'   identical(rdf[["pass"]], FALSE)
#' )
#' rdf <- dbc::report_is_one_of(
#'   "hello there",
#'   funs = list(dbc::report_is_NULL, dbc::report_is_character_nonNA_vector)
#' )
#' stopifnot(
#'   nrow(rdf) == 1L,
#'   identical(rdf[["pass"]], TRUE)
#' )
report_is_one_of <- function(x, x_nm = NULL, call = NULL, funs) {
  # @codedoc_comment_block news("dbc::report_is_one_of", "2023-07-04", "0.4.14")
  # 
  # @codedoc_comment_block news("dbc::report_is_one_of", "2023-07-04", "0.4.14")
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_is_one_of_call <- match.call()
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  funs <- as.list(funs)
  funs <- lapply(seq_along(funs), function(i) {
    fun <- funs[[i]]
    fun <- get_function(fun)
    dbc::assert_prod_input_is_function_with_required_argument_names(
      fun,
      x_nm = paste0("funs[[", i, "]]"),
      call = report_is_one_of_call,
      required_argument_names = c("x", "x_nm", "call")
    )
    fun
  })
  report_df <- do.call(rbind, lapply(seq_along(funs), function(i) {
    expr <- substitute(funs[[i]](x = x, x_nm = x_nm, call = call), list(i = i))
    report_df <- eval(expr)
    dbc::assert_prod_interim_is_report_df(
      report_df,
      x_nm = deparse1(expr),
      call = report_is_one_of_call
    )
    aggregate_report_df(report_df, pass = "all")
  }), quote = TRUE)
  return(aggregate_report_df(report_df, pass = "any"))
}
