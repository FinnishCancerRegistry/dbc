




#' @rdname assertions
#' @export
report_is_Date_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_Date(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_Date_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_Date(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_Date_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_Date(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_Date_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_Date(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_Date_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_Date(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_Date_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_Date(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_character_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_character(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_character_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_character(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_character_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_character(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_character_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_character(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_character_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_character(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_character_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_character(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_double_nonNA_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_double(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_factor_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_factor(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_factor_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_factor(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_factor_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_factor(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_factor_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_factor(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_factor_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_factor(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_factor_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_factor(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_integer_nonNA_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_integer(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_logical_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_logical(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_logical_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_logical(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_logical_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_logical(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_logical_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_logical(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_logical_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_logical(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_logical_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_logical(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_gtezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_gtezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_gtezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_gtzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_gtzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_gtzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_gtzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_ltezero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_ltezero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_ltezero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltezero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_ltzero_atom <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_atom(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_ltzero_matrix <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_matrix(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

#' @rdname assertions
#' @export
report_is_number_nonNA_ltzero_vector <- function(x, x_nm = NULL, call = NULL) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call = call, env = parent.frame(1L))
  out <- rbind(
    report_is_number(x = x, x_nm = x_nm, call = call), 
    report_is_nonNA(x = x, x_nm = x_nm, call = call), 
    report_is_ltzero(x = x, x_nm = x_nm, call = call), 
    report_is_vector(x = x, x_nm = x_nm, call = call)
  )
  return(out)
}

