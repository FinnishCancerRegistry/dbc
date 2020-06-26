# this script was generated automatically. do not edit by hand!





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_atom_is_in_set <- function(
  x, 
  x_nm = NULL, 
  set
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_atom_is_in_set"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_dir_exists <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_dir_exists"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_file_exists <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_file_exists"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_class <- function(
  x, 
  x_nm = NULL, 
  required_class
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_has_class"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_length <- function(
  x, 
  x_nm = NULL, 
  expected_length
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_has_length"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_names <- function(
  x, 
  x_nm = NULL, 
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_has_names"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_one_of_classes <- function(
  x, 
  x_nm = NULL, 
  classes
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_has_one_of_classes"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_has_only_names <- function(
  x, 
  x_nm = NULL, 
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_has_only_names"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_inherits <- function(
  x, 
  x_nm = NULL, 
  required_class
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_inherits"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_between_exclusive <- function(
  x, 
  x_nm = NULL, 
  lo, 
  hi
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_between_exclusive"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_between_inclusive <- function(
  x, 
  x_nm = NULL, 
  lo, 
  hi
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_between_inclusive"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_call <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_call"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_character_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_character_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.frame <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_data.frame"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.frame_with_required_names <- function(
  x, 
  x_nm = NULL, 
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_data.frame_with_required_names"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.table <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_data.table"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data.table_with_required_names <- function(
  x, 
  x_nm = NULL, 
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_data.table_with_required_names"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data_table <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_data_table"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_data_table_with_required_names <- function(
  x, 
  x_nm = NULL, 
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_data_table_with_required_names"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_Date_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_Date_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_gtezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_gtezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_gtezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_gtzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_gtzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_gtzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_gtzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_ltezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_ltezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_ltezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_ltzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_ltzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_ltzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_ltzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_gtezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_gtezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_gtezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_gtzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_gtzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_gtzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_gtzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_ltezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_ltezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_ltezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_ltzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_ltzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_ltzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_ltzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_double_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_double_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_environment <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_environment"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_expression <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_expression"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_factor_with_levels <- function(
  x, 
  x_nm = NULL, 
  expected_levels
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_factor_with_levels"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_function <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_function"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gt <- function(
  x, 
  x_nm = NULL, 
  lo
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_gt"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gte <- function(
  x, 
  x_nm = NULL, 
  lo
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_gte"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gtezero <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_gtezero"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_gtzero <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_gtzero"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_gtezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_gtezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_gtezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_gtzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_gtzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_gtzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_gtzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_ltezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_ltezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_ltezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_ltzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_ltzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_ltzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_ltzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_gtezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_gtezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_gtezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_gtzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_gtzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_gtzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_gtzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_ltezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_ltezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_ltezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_ltzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_ltzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_ltzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_ltzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_integer_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_integer_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_language_object <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_language_object"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_list <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_list"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_logical_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_logical_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_lt <- function(
  x, 
  x_nm = NULL, 
  lo, 
  hi
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_lt"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_lte <- function(
  x, 
  x_nm = NULL, 
  hi
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_lte"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_ltezero <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_ltezero"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_ltzero <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_ltzero"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_name <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_name"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_named <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_named"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_named_list <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_named_list"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_nonNA <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_nonNA"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_NULL <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_NULL"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_gtezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_gtezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_gtezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_gtzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_gtzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_gtzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_gtzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_ltezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_ltezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_ltezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_ltzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_ltzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_ltzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_ltzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_gtezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_gtezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_gtezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_gtzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_gtzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_gtzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_gtzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltezero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_ltezero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltezero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_ltezero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltezero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_ltezero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltzero_atom <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_ltzero_atom"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltzero_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_ltzero_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_ltzero_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_ltzero_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_matrix <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_matrix"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_nonNA_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_nonNA_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_number_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_number_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_numeric <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_numeric"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_of_length <- function(
  x, 
  x_nm = NULL, 
  expected_length
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_of_length"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_uniquely_named <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_uniquely_named"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_uniquely_named_list <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_uniquely_named_list"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_is_vector <- function(
  x, 
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_is_vector"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





# this function was generated automatically. do not edit by hand!
#' @rdname assertions
#' @export
test_vector_elems_are_in_set <- function(
  x, 
  x_nm = NULL, 
  set
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_fun_nm <- "report_vector_elems_are_in_set"
  arg_list <- mget(names(formals(report_fun_nm)))
  report_df <- call_with_arg_list(report_fun_nm, arg_list)
  return(all(report_df[["pass"]]))
}





