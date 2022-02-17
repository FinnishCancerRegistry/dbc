# this script was generated automatically. do not edit by hand!





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_all_are_TRUE <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_all_are_TRUE(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_atom_is_in_set <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  set
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_atom_is_in_set(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    set = set
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_dir_exists <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_dir_exists(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_file_exists <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_file_exists(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_class <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_class
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_class(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_class = required_class
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_length <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  expected_length
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_length(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    expected_length = expected_length
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_names <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_names
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_names(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_names = required_names
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_no_duplicates <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_no_duplicates(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_one_of_classes <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  classes
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_one_of_classes(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    classes = classes
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_only_names <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_names
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_only_names(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_names = required_names
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_only_valid_observations <- function(
  x, 
  x_nm = NULL, 
  expressions, 
  fail_messages = NULL, 
  pass_messages = NULL, 
  col_nms = names(x), 
  col_nm_set_list = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_has_only_valid_observations(
    x = x, 
    x_nm = x_nm, 
    expressions = expressions, 
    fail_messages = fail_messages, 
    pass_messages = pass_messages, 
    col_nms = col_nms, 
    col_nm_set_list = col_nm_set_list, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_inherits <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_class
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_inherits(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_class = required_class
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_assertion_type <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_assertion_type(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_between_exclusive <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  lo, 
  hi
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_between_exclusive(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    lo = lo, 
    hi = hi
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_between_inclusive <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  lo, 
  hi
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_between_inclusive(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    lo = lo, 
    hi = hi
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_call <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_call(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_character_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.frame <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_data.frame(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.frame_with_required_names <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_names
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_data.frame_with_required_names(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_names = required_names
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.table <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_data.table(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.table_with_required_names <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_names
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_data.table_with_required_names(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_names = required_names
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data_table <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_data_table(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data_table_with_required_names <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_names
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_data_table_with_required_names(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_names = required_names
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_Date_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_gtezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_gtezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_gtezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_gtzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_gtzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_gtzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_ltezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_ltezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_ltezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_ltzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_ltzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_ltzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_gtezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_gtezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_gtezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_gtzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_gtzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_gtzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_ltezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_ltezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_ltezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_ltzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_ltzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_ltzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_double_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_environment <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_environment(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_expression <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_expression(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_with_levels <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  expected_levels
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_factor_with_levels(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    expected_levels = expected_levels
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_function <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_function(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_function_with_required_argument_names <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  required_argument_names
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_function_with_required_argument_names(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    required_argument_names = required_argument_names
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gt <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  lo
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_gt(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    lo = lo
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gte <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  lo
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_gte(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    lo = lo
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gtezero <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_gtezero(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gtzero <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_gtzero(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_gtezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_gtezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_gtezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_gtzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_gtzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_gtzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_ltezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_ltezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_ltezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_ltzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_ltzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_ltzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_gtezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_gtezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_gtezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_gtzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_gtzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_gtzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_ltezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_ltezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_ltezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_ltzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_ltzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_ltzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_integer_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_language_object <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_language_object(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_list <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_list(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_logical_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_lt <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  hi
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_lt(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    hi = hi
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_lte <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  hi
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_lte(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    hi = hi
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_ltezero <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_ltezero(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_ltzero <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_ltzero(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_name <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_name(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_named <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_named(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_named_list <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_named_list(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_nonNA <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_nonNA(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_NULL <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_NULL(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_gtezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_gtezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_gtezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_gtzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_gtzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_gtzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_ltezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_ltezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_ltezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_ltzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_ltzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_ltzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_gtezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_gtezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_gtezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_gtzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_gtzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_gtzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltezero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_ltezero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltezero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_ltezero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltezero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_ltezero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltzero_atom <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_ltzero_atom(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltzero_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_ltzero_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltzero_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_ltzero_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_matrix <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_matrix(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_nonNA_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_number_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_numeric <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_numeric(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_of_length <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  expected_length
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_of_length(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    expected_length = expected_length
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_TRUE <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_TRUE(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_uniquely_named <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_uniquely_named(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_uniquely_named_list <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_uniquely_named_list(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_vector <- function(
  x, 
  x_nm = NULL, 
  call = NULL
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_is_vector(
    x = x, 
    x_nm = x_nm, 
    call = call
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_match_regex <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  grepl.arg.list = list()
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_match_regex(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    grepl.arg.list = grepl.arg.list
  )
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_vector_elems_are_in_set <- function(
  x, 
  x_nm = NULL, 
  call = NULL, 
  set
) {
  is.null(x) # trigger lazy eval -> no "restarting interrupted promise evaluation"
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- report_vector_elems_are_in_set(
    x = x, 
    x_nm = x_nm, 
    call = call, 
    set = set
  )
  return(all(report_df[["pass"]]))
}





