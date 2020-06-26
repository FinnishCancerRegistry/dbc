# this script was generated automatically. do not edit by hand!





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_atom_is_in_set <- function(x, x_nm = NULL, set) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x %in% set"
  )
  fail_msg_set <- c(
    "${x_nm} = ${x} was not in set of expected values (first ten): ${deparse(utils::head(set, 10L))}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_dir_exists <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "length(bad_dirs <- x[!dir.exists(x)]) == 0L"
  )
  fail_msg_set <- c(
    "no such directory/directories: ${bad_dirs}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_file_exists <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "length(bad_files <- x[!file.exists(x)]) == 0L"
  )
  fail_msg_set <- c(
    "no such file(s): ${bad_files}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_class <- function(x, x_nm = NULL, required_class) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, required_class)"
  )
  fail_msg_set <- c(
    "expected ${x_nm} to have class ${required_class}, but it had class(es) ${deparse(class(x))}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_length <- function(x, x_nm = NULL, expected_length) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "length(x) == expected_length"
  )
  fail_msg_set <- c(
    "${x_nm} had length ${length(x)} instead of ${expected_length}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_names <- function(x, x_nm = NULL, required_names) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("!is.null(names(x))", "length(miss_nms <- setdiff(required_names, names(x))) == 0L" )
  )
  fail_msg_set <- c(
    c(NA, "${x_nm} did not have the following expected names: ${deparse(miss_nms)}" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_one_of_classes <- function(x, x_nm = NULL, classes) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, classes)"
  )
  fail_msg_set <- c(
    "expected ${x_nm} to have class ${classes}, but it had class(es) ${deparse(class(x))}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_only_names <- function(x, x_nm = NULL, required_names) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("!is.null(names(x))", "length(miss_nms <- setdiff(required_names, names(x))) == 0L",  "length(extra_nms <- setdiff(names(x), required_names)) == 0L" )
  )
  fail_msg_set <- c(
    c(NA, "${x_nm} did not have the following expected names: ${deparse(miss_nms)}",  "${x_nm} had the following unexpected names: ${deparse(extra_nms)}" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_inherits <- function(x, x_nm = NULL, required_class) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, required_class)"
  )
  fail_msg_set <- c(
    "expected ${x_nm} to have class ${required_class}, but it had class(es) ${deparse(class(x))}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_atom <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "length(x) == 1L"
  )
  fail_msg_set <- c(
    "length of ${x_nm} was ${length(x)} instead of 1"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_between_exclusive <- function(x, x_nm = NULL, lo, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "!dbc::is_between_exclusive(x = x, lo = lo, hi = hi)"
  )
  fail_msg_set <- c(
    "${n_fail} elements were outside exclusive bounds ${lo}, ${hi}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_between_inclusive <- function(x, x_nm = NULL, lo, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "dbc::is_between_inclusive(x = x, lo = lo, hi = hi)"
  )
  fail_msg_set <- c(
    "${n_fail} elements were outside inclusive bounds ${lo}, ${hi}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_call <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.call(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.character(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.frame <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.data.frame(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.frame_with_required_names <- function(x, x_nm = NULL, required_names) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("is.data.frame(x)", "length(miss_nms <- setdiff(required_names, names(x))) == 0L" )
  )
  fail_msg_set <- c(
    c(NA, "${x_nm} did not have the following expected names: ${deparse(miss_nms)}" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.table <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, 'data.table')"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.table_with_required_names <- function(x, x_nm = NULL, required_names) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("inherits(x, 'data.table')", "length(miss_nms <- setdiff(required_names, names(x))) == 0L" )
  )
  fail_msg_set <- c(
    c(NA, "${x_nm} did not have the following expected names: ${deparse(miss_nms)}" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data_table <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, 'data.table')"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data_table_with_required_names <- function(x, x_nm = NULL, required_names) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("inherits(x, 'data.table')", "length(miss_nms <- setdiff(required_names, names(x))) == 0L" )
  )
  fail_msg_set <- c(
    c(NA, "${x_nm} did not have the following expected names: ${deparse(miss_nms)}" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, 'Date')"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.double(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_environment <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.environment(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_expression <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.expression(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.factor(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_with_levels <- function(x, x_nm = NULL, expected_levels) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("is.factor(x)", "length(extra_levels <- setdiff(levels(x), expected_levels)) == 0L",  "length(miss_levels <- setdiff(expected_levels, levels(x))) == 0L" )
  )
  fail_msg_set <- c(
    c("${x_nm} is not a factor", "factor ${x_nm} had these unexpected levels: ${deparse(extra_levels)}",  "factor ${x_nm} did not have these expected levels: ${deparse(miss_levels)}" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_function <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.function(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gt <- function(x, x_nm = NULL, lo) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x > lo"
  )
  fail_msg_set <- c(
    "${n_fail} elements were <= ${lo}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gte <- function(x, x_nm = NULL, lo) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x >= lo"
  )
  fail_msg_set <- c(
    "${n_fail} elements were < ${lo}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gtezero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x >= 0"
  )
  fail_msg_set <- c(
    "${n_fail} elements were < 0"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gtzero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x > 0"
  )
  fail_msg_set <- c(
    "${n_fail} elements were <= 0}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.integer(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_language_object <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.language(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "inherits(x, 'list')"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.logical(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_lt <- function(x, x_nm = NULL, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x < hi"
  )
  fail_msg_set <- c(
    "${n_fail} elements were >= ${hi}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_lte <- function(x, x_nm = NULL, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x <= hi"
  )
  fail_msg_set <- c(
    "${n_fail} elements were > ${hi}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_ltezero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x <= 0"
  )
  fail_msg_set <- c(
    "${n_fail} elements were > 0"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_ltzero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "x < 0"
  )
  fail_msg_set <- c(
    "${n_fail} elements were >= 0"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_matrix <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.matrix(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_name <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.name(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_named <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "!is.null(names(x))"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_named_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("inherits(x, 'list')", "!is.null(names(x))")
  )
  fail_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_nonNA <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "!is.na(x)"
  )
  fail_msg_set <- c(
    "${x_nm} had ${n_fail} NA values - none are allowed"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_NULL <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.null(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.numeric(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_numeric <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "is.numeric(x)"
  )
  fail_msg_set <- c(
    NA_character_
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_of_length <- function(x, x_nm = NULL, expected_length) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "length(x) == expected_length"
  )
  fail_msg_set <- c(
    "${x_nm} had length ${length(x)} instead of ${expected_length}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_uniquely_named <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("!is.null(names(x))", "(n_unique_names <- length(unique(names(x)))) == length(x)" )
  )
  fail_msg_set <- c(
    c(NA, "not every element of ${x_nm} has a different name")
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_uniquely_named_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("inherits(x, 'list')", "!is.null(names(x))", "(n_unique_names <- length(unique(names(x)))) == length(x)" )
  )
  fail_msg_set <- c(
    c(NA, NA, "not every element of ${x_nm} has a different name" )
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_vector <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    c("is.null(dim(x))", "!is.list(x)")
  )
  fail_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  pass_msg_set <- c(
    c(NA_character_, NA_character_)
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_vector_elems_are_in_set <- function(x, x_nm = NULL, set) {
  x_nm <- handle_x_nm_arg(x_nm)
  fun_eval_env <- environment()
  test_set <- c(
    "in_set <- x %in% set"
  )
  fail_msg_set <- c(
    "some values of ${x_nm} were not in set of expected values (first ten bad values): ${deparse(utils::head(unique(x[!in_set]), 10L))}"
  )
  pass_msg_set <- c(
    NA_character_
  )
  report_df <- tests_to_report(
    tests = test_set,
    fail_messages = fail_msg_set,
    pass_messages = pass_msg_set,
    env = fun_eval_env
  )
  return(report_df)
}



