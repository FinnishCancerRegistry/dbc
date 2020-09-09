
#' @rdname assertions
#' @export
#' @param funs `[character, list]` (mandatory, no default)
#'
#' report functions that return a report (data.frame);
#' - `character`: names of functions that can be found by `[match.fun]`
#' - `list`: list of functions
report_is_one_of <- function(x, x_nm = NULL, funs) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    inherits(funs, c("list", "character"))
  )
  funs <- lapply(funs, match.fun)
  report_df <- do.call(rbind, lapply(funs, function(fun) {
    arg_list <- formals(fun)
    arg_list[c("x", "x_nm")] <- list(x = quote(x), x_nm = quote(x_nm))
    do.call(fun, arg_list)
  }))

  return(report_df)
}
