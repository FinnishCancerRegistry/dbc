

interpolate <- function(x, env = parent.frame(1L)) {
  stopifnot(
    is.character(x),
    length(x) == 1,
    is.environment(env)
  )
  # @codedoc_comment_block dbc:::interpolate
  # Given the *interpolation environment* and a string, interpolation is
  # performed as follows:
  #
  # 1. Expressions to interpolate in a string are detected using regex
  #    `"[$][{][^{]+[}]"`. E.g. in `"A total of $ {1 + 1} things"`
  #    substring `$ {1 + 1}` is detected (without the whitespace after $).
  #    Multiline expressions are not detected.
  #    Expressions that contain `}` (e.g. `"One is $ {{my_var <- 1; my_var}}"`)
  #    will not be parsed correctly.
  # @codedoc_comment_block dbc:::interpolate
  m <- gregexpr(pattern = "[$][{][^{]+[}]", text = x, perl = TRUE)
  has_nothing_to_interpolate <- length(m) == 1L && identical(m[[1]], -1L)
  if (has_nothing_to_interpolate) {
    return(x)
  }

  expr_strings_by_x_elem <- regmatches(x = x, m = m)
  values <- lapply(expr_strings_by_x_elem, function(expr_string_vec) {
    # @codedoc_comment_block dbc:::interpolate
    # 2. Each expression substring is evaluated in the
    #    *interpolation environment*, via
    #    `eval(parse(text = expression_substring)[[1]])`.
    #    Evaluation is done within a `[tryCatch]` call; if an error or warning
    #    is caught, interpolation fails, and the original expression substring
    #    is used as the result of the evaluation. Otherwise the result is
    #    what `eval` gives.
    # @codedoc_comment_block dbc:::interpolate
    expr_string_vec <- substr(expr_string_vec, 3L, nchar(expr_string_vec) - 1L)
    vapply(expr_string_vec, function(expression_substring) {
      expr <- parse(text = expression_substring)[[1L]]
      evaled <- tryCatch(
        eval(expr, envir = env),
        error = function(e) expression_substring,
        warning = function(w) expression_substring
      )
      paste0(as.character(evaled), collapse = "")
    }, character(1L))
  })
  # @codedoc_comment_block dbc:::interpolate
  # 3. Each expression substring in the original string is substituted with
  #    its result. This modified string is returned.
  # @codedoc_comment_block dbc:::interpolate
  regmatches(x = x, m = m) <- values
  x
}
