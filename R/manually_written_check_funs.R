




#' @title Assertions
#' @description
#' Collection of assertion functions which raise an error if the test does not
#' pass.
#' @name assertions
#' @param x R object to be tested (mandatory, no default)
#' @param x_nm `[NULL, character]` (optional, default `NULL`)
#'
#' the name of the object `x` to mention in possible error message.
#' - `NULL`: taken as `deparse(substitute(x))`
#' - `character`: the name as a string
#' @param call `[language, NULL]` (optional, default `NULL`)
#'
#' - `language`: an R language object such as one produced by [match.call] or
#'   `quote`; this call will be reported in an error
#' - `NULL`: the call is attempted to be inferred
#' @param lo `[number]` (mandatory, no default)
#' lower bound for `x`
#'
#' @param hi `[number]` (mandatory, no default)
#' upper bound for `x`
#' @param set `[any vector]` (mandatory, no default)
#' set of values to compare to
#' @param required_class `[character]` (mandatory, no default)
#' class that object must have
#' @param expected_length `[integer]` (mandatory, no default)
#' length object must have
#' @param required_names `[character]` (mandatory, no default)
#' set of names object must have
#' @param classes `[character]` (mandatory, no default)
#' one or more classes; object must have at least one of these as class
#' @param expected_levels `[character]` (mandatory, no default)
#' set of levels factor is required to have
#'
NULL


#' @title Between
#' @description
#' Test if elements are between upper and lower bounds.
#' @param x `[R object]` (mandatory, no default)
#' e.g. numeric vector; each element is tested for being between `lo` and `hi`
#' @param lo `[R object]` (mandatory, no default)
#' lower bound of length 1 or `length(x)`
#' @param hi `[R object]` (mandatory, no default)
#' upper bound of length 1 or `length(x)`
#' @param inclusive `[logical]` (optional, default `TRUE`)
#' if `TRUE`, test is `lo <= x & x <= hi` and else `lo < x & x < hi`.
#' @export
is_between <- function(x, lo, hi, inclusive = TRUE) {
  stopifnot(
    length(inclusive) == 1L,
    inclusive %in% c(TRUE, FALSE),
    length(lo) %in% c(1L, length(x)),
    length(hi) %in% c(1L, length(x))
  )
  if (inclusive) {
    lo <= x & x <= hi
  } else {
    lo < x & x < hi
  }
}

#' @describeIn is_between `is_between` with `inclusive = TRUE`
#' @export
is_between_inclusive <- function(x, lo, hi) {
  is_between(x, lo, hi, inclusive = TRUE)
}

#' @describeIn is_between `is_between` with `inclusive = FALSE`
#' @export
is_between_exclusive <- function(x, lo, hi) {
  is_between(x, lo, hi, inclusive = FALSE)
}









