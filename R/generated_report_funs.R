# this script was generated automatically. do not edit by hand!





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_all_are_TRUE <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "all(x %in% TRUE)"
  )
  fail_message_set <- c(
    "Not all elements of ${deparse(x_nm)} were TRUE"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_atom_is_in_set <- function(x, x_nm = NULL, call = NULL, set) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_atom(x)", 
    "dbc::test_is_vector(set)", 
    "x %in% set"
  )
  fail_message_set <- c(
    "NA", 
    "NA", 
    "object ${deparse(x_nm)} = ${x} was not in set of expected values (first ten): ${deparse(utils::head(set, 10L))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_data_table_has_no_duplicates <- function(x, x_nm = NULL, call = NULL, by = names(x)) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "!(dup <- duplicated(x, by = by))"
  )
  fail_message_set <- c(
    "In total ${n_fail} rows of data.table ${deparse1(x_nm)} were duplicates (by ${deparse1(by)}); first five row numbers that were duplicated: ${deparse1(utils::head(which(dup), 5))}"
  )
  pass_message_set <- c(
    "data.table ${deparse1(x_nm)} had no duplicates (by ${deparse1(by)})"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_dir_exists <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(bad_dirs <- x[!dir.exists(x)]) == 0L"
  )
  fail_message_set <- c(
    "the following directory/directories in object ${deparse(x_nm)} do not exist: ${deparse(bad_dirs)}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_file_exists <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(bad_files <- x[!file.exists(x)]) == 0L"
  )
  fail_message_set <- c(
    "the following file(s) in object ${deparse(x_nm)} do not exist: ${bad_files}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_class <- function(x, x_nm = NULL, call = NULL, required_class) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_atom(required_class)", 
    "inherits(x, required_class)"
  )
  fail_message_set <- c(
    "Internal error: expected required_class to be a non-NA character string (vector of length one), but required_class = ${deparse(required_class)} ", 
    "expected object ${deparse(x_nm)} to have class ${required_class}, but it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_length <- function(x, x_nm = NULL, call = NULL, expected_length) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == expected_length"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} had length ${length(x)} instead of ${expected_length}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_names <- function(x, x_nm = NULL, call = NULL, required_names) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(required_names)", 
    "!is.null(names(x))", 
    "length(miss_nms <- setdiff(required_names, names(x))) == 0L"
  )
  fail_message_set <- c(
    "NA", 
    "object ${deparse(x_nm)} did not have any names", 
    "object ${deparse(x_nm)} did not have the following expected names: ${deparse(miss_nms)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_no_duplicates <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "!duplicated(x)"
  )
  fail_message_set <- c(
    "In total ${n_fail} elements of object/param ${deparse(x_nm)} were duplicates; first five values that were duplicated: ${deparse(utils::head(unique(x[duplicated(x)]), 5L))}"
  )
  pass_message_set <- c(
    "object ${deparse(x_nm)} had no duplicates"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_one_of_classes <- function(x, x_nm = NULL, call = NULL, classes) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(classes)", 
    "inherits(x, classes)"
  )
  fail_message_set <- c(
    "NA", 
    "expected object ${deparse(x_nm)} to have class ${classes}, but it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_has_only_names <- function(x, x_nm = NULL, call = NULL, required_names) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(required_names)", 
    "!is.null(names(x))", 
    "length(miss_nms <- setdiff(required_names, names(x))) == 0L", 
    "length(extra_nms <- setdiff(names(x), required_names)) == 0L"
  )
  fail_message_set <- c(
    "NA", 
    "object ${deparse(x_nm)} did not have any names", 
    "object ${deparse(x_nm)} did not have the following expected names: ${deparse(miss_nms)}", 
    "object ${deparse(x_nm)} had the following unexpected names: ${deparse(extra_nms)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_inherits <- function(x, x_nm = NULL, call = NULL, required_class) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_atom(required_class)", 
    "inherits(x, required_class)"
  )
  fail_message_set <- c(
    "Internal error: expected required_class to be a non-NA character string (vector of length one), but required_class = ${deparse(required_class)} ", 
    "expected object ${deparse(x_nm)} to have class ${required_class}, but it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_assertion_type <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_atom(x)", 
    "dbc::test_is_character(x)", 
    "dbc::test_is_nonNA(x)", 
    "dbc::test_atom_is_in_set(x, set = dbc::assertion_types())"
  )
  fail_message_set <- c(
    "${deparse(x_nm)} was not atomic", 
    "${deparse(x_nm)} was not a string", 
    "${deparse(x_nm)} was NA", 
    "${deparse(x_nm)} was not one of the strings given by dbc::assertion_types()"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_between_exclusive <- function(x, x_nm = NULL, call = NULL, lo, hi) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_number_nonNA_vector(lo)", 
    "dbc::test_is_number_nonNA_vector(hi)", 
    "!dbc::is_between_exclusive(x = x, lo = lo, hi = hi)"
  )
  fail_message_set <- c(
    "NA", 
    "NA", 
    "${n_fail} elements of ${deparse(x_nm)} were outside exclusive bounds ${lo}, ${hi}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_between_inclusive <- function(x, x_nm = NULL, call = NULL, lo, hi) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_number_nonNA_vector(lo)", 
    "dbc::test_is_number_nonNA_vector(hi)", 
    "dbc::is_between_inclusive(x = x, lo = lo, hi = hi)"
  )
  fail_message_set <- c(
    "NA", 
    "NA", 
    "${n_fail} elements of ${deparse(x_nm)} were outside inclusive bounds ${lo}, ${hi}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_call <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.call(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a call object; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.character(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.character(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.character(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.character(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.character(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.character(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_character_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.character(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class character; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.frame <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.data.frame(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a data.frame; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.frame_with_required_names <- function(x, x_nm = NULL, call = NULL, required_names) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(required_names)", 
    "is.data.frame(x)", 
    "length(miss_nms <- setdiff(required_names, names(x))) == 0L"
  )
  fail_message_set <- c(
    "NA", 
    "object ${deparse(x_nm)} was not a data.frame; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} did not have the following expected columns: ${deparse(miss_nms)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.table <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'data.table')"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a data.table; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data.table_with_required_names <- function(x, x_nm = NULL, call = NULL, required_names) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(required_names)", 
    "inherits(x, 'data.table')", 
    "length(miss_nms <- setdiff(required_names, names(x))) == 0L"
  )
  fail_message_set <- c(
    "NA", 
    "object ${deparse(x_nm)} was not a data.table; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} did not have the following expected columns: ${deparse(miss_nms)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data_table <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'data.table')"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a data.table; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_data_table_with_required_names <- function(x, x_nm = NULL, call = NULL, required_names) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(required_names)", 
    "inherits(x, 'data.table')", 
    "length(miss_nms <- setdiff(required_names, names(x))) == 0L"
  )
  fail_message_set <- c(
    "NA", 
    "object ${deparse(x_nm)} was not a data.table; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} did not have the following expected columns: ${deparse(miss_nms)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'Date')"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "inherits(x, 'Date')"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'Date')", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "inherits(x, 'Date')", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'Date')", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'Date')", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_Date_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'Date')", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class Date; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x >= 0"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x >= 0", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x >= 0", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x > 0"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x > 0", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x > 0", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x <= 0"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x <= 0", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x <= 0", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x < 0"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x < 0", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x < 0", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x >= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x >= 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x >= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x > 0", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x > 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x > 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x <= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x <= 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x <= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.double(x)", 
    "x < 0", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x < 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "x < 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_double_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.double(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class numeric; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_environment <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.environment(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not an environment object"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_expression <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.expression(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not an R expression object; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.factor(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.factor(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.factor(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.factor(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.factor(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.factor(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.factor(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class factor; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_factor_with_levels <- function(x, x_nm = NULL, call = NULL, expected_levels) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_character_nonNA_vector(expected_levels)", 
    "is.factor(x)", 
    "length(extra_levels <- setdiff(levels(x), expected_levels)) == 0L", 
    "length(miss_levels <- setdiff(expected_levels, levels(x))) == 0L"
  )
  fail_message_set <- c(
    "NA", 
    "object ${deparse(x_nm)} is not a factor; instead it had class(es) ${deparse(class(x))}", 
    "factor object ${deparse(x_nm)} had these unexpected levels: ${deparse(extra_levels)}", 
    "factor object ${deparse(x_nm)} did not have these expected levels: ${deparse(miss_levels)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_function <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.function(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a function; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_function_with_required_argument_names <- function(x, x_nm = NULL, call = NULL, required_argument_names) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.function(x)", 
    "all(required_argument_names %in% names(formals(x)))"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a function", 
    "object ${deparse(x_nm)} did not have all required arguments ${deparse(required_argument_names)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gt <- function(x, x_nm = NULL, call = NULL, lo) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > lo"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= ${lo}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gte <- function(x, x_nm = NULL, call = NULL, lo) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= lo"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < ${lo}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gtezero <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_gtzero <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.integer(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x >= 0", 
    "is.integer(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.integer(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.integer(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x > 0", 
    "is.integer(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.integer(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.integer(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.integer(x)", 
    "x <= 0"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x <= 0", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x <= 0", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.integer(x)", 
    "x < 0"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x < 0", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x < 0", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.integer(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x >= 0", 
    "is.integer(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.integer(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.integer(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x > 0", 
    "is.integer(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.integer(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.integer(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.integer(x)", 
    "x <= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x <= 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x <= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.integer(x)", 
    "x < 0", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x < 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "x < 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_integer_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.integer(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class integer; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_language_object <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.language(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not an R language object; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_list <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'list')"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class list"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.logical(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.logical(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.logical(x)", 
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.logical(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.logical(x)", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.logical(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_logical_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.logical(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class logical; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_lt <- function(x, x_nm = NULL, call = NULL, hi) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x < hi"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were >= ${hi}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_lte <- function(x, x_nm = NULL, call = NULL, hi) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x <= hi"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were > ${hi}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_ltezero <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x <= 0"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were > 0"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_ltzero <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x < 0"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were >= 0"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.matrix(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a matrix"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_name <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.name(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a name object; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_named <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "!is.null(names(x))"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} did not have any names"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_named_list <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'list')", 
    "!is.null(names(x))"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class list", 
    "object ${deparse(x_nm)} did not have any names"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_nonNA <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "if (!is.function(x)) !is.na(x) else TRUE"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_NULL <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.null(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not NULL"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x >= 0", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.matrix(x)", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x > 0", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.matrix(x)", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x <= 0", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x <= 0", 
    "is.matrix(x)", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x <= 0", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x < 0", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x < 0", 
    "is.matrix(x)", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x < 0", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.matrix(x)", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x >= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x >= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were < 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x > 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x > 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were <= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x <= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x <= 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x <= 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were > 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == 1L", 
    "x < 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "length of object ${deparse(x_nm)} was ${length(x)} instead of 1", 
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x < 0", 
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x < 0", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were >= 0", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.matrix(x)", 
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a matrix", 
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "if (!is.function(x)) !is.na(x) else TRUE", 
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} had ${n_fail} NA values - none are allowed", 
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_number_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.numeric(x)", 
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not a number; instead it had class(es) ${deparse(class(x))}", 
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_numeric <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.numeric(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not numeric; instead it had class(es) ${deparse(class(x))}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_of_length <- function(x, x_nm = NULL, call = NULL, expected_length) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "length(x) == expected_length"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} had length ${length(x)} instead of ${expected_length}"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_report_df <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.data.frame(x)", 
    "length(miss_col_nms <- setdiff(names(get_report_df_template()), names(x))) == 0L"
  )
  fail_message_set <- c(
    "${deparse(x_nm)} was not a data.frame", 
    "Following columns were expected but not in ${deparse(x_nm)}: ${deparse(miss_col_nms)}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_TRUE <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "x %in% TRUE"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} were FALSE or NA"
  )
  pass_message_set <- c(
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_uniquely_named <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "!is.null(names(x))", 
    "(n_unique_names <- length(unique(names(x)))) == length(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} did not have any names", 
    "not every element of object ${deparse(x_nm)} has a different name"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_uniquely_named_list <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "inherits(x, 'list')", 
    "!is.null(names(x))", 
    "(n_unique_names <- length(unique(names(x)))) == length(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} was not of class list", 
    "object ${deparse(x_nm)} did not have any names", 
    "not every element of object ${deparse(x_nm)} has a different name"
  )
  pass_message_set <- c(
    "NA", 
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_is_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "is.null(dim(x))", 
    "!is.list(x)"
  )
  fail_message_set <- c(
    "object ${deparse(x_nm)} had dimensions but was expected to have none", 
    "object ${deparse(x_nm)} was a list"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_match_regex <- function(x, x_nm = NULL, call = NULL, grepl.arg.list = list()) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "{grepl.arg.list[['x']] <- x; t <- do.call(grepl, grepl.arg.list); t}"
  )
  fail_message_set <- c(
    "${n_fail} elements of ${deparse(x_nm)} did not match regex ${deparse(grepl.arg.list[['pattern']])}"
  )
  pass_message_set <- c(
    "All elements of ${deparse(x_nm)} matched regex ${deparse(grepl.arg.list[['pattern']])}"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
report_vector_elems_are_in_set <- function(x, x_nm = NULL, call = NULL, set) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  if (missing(x)) {
    stop(simpleError(
      message = paste0(
        "Argument ", x_nm, " was missing --- it has no default so ",
        "some value must be supplied!"
      ),
      call = call
    ))
  }
  report_env <- environment()
  expression_set <- c(
    "dbc::test_is_vector(x)", 
    "in_set <- x %in% set"
  )
  fail_message_set <- c(
    "NA", 
    "some values of object ${deparse(x_nm)} were not in set of expected values. First ten bad values: ${deparse1(utils::head(unique(x[!in_set]), 10L))}. First ten elements in set of expected values: ${deparse1(utils::head(set, 10L))}"
  )
  pass_message_set <- c(
    "NA", 
    "NA"
  )
  report_df <- dbc::expressions_to_report(
    expressions = expression_set,
    fail_messages = fail_message_set,
    pass_messages = pass_message_set,
    env = report_env, 
    call = call
  )
  return(report_df)
}



