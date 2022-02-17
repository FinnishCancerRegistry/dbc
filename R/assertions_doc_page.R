
#' @title Assertions
#' @description
#' Collection of assertion functions which raise an error if the test does not
#' pass.
#' @name assertions
#' @template arg_x
#' @template arg_x_nm
#' @template arg_call
#' @eval arg_assertion_type_docs()
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
#' @param required_argument_names `[character]` (mandatory, no default)
#'
#' names of arguments that function `x` must have
#' @param grepl.arg.list `[list]` (default `list()`)
#'
#' list of (optional) arguments passed to `[grepl]`
#'
NULL
