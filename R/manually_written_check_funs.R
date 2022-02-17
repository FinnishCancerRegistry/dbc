





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









