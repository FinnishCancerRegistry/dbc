




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
    df_class <- dbc::report_inherits(
      x = x,
      x_nm = x_nm,
      call = call,
      required_class = class(template)
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
        seq_along(x),
        function(i) {
          ref_nm <- i
          if (!is.null(names(template))) {
            ref_nm <- paste0("\"", names(template)[i], "\"")
          }
          report_is_like_template(
            x = x[[i]],
            x_nm = paste0(x_nm, "[[", ref_nm, "]]"),
            call = call,
            template = template[[i]],
            compare = compare
          )
        }
      ),
      quote = TRUE
    )
    df <- rbind(df, df_recursive)
  }

  return(df)
}








