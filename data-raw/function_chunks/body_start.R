# @codedoc_comment_block news("dbc", "2023-08-16", "0.4.16")
# All generated assertion, report and test functions now check whether
# `x` is missing and raise an informative error. There were edge cases
# where `x` was attempted to be evaluated only in a call to `eval` which
# resulted in cryptic error messages --- now those can no longer occur.
# @codedoc_comment_block news("dbc", "2023-08-16", "0.4.16")
x_nm <- dbc::handle_arg_x_nm(x_nm)
call <- dbc::handle_arg_call(call)
if (missing(x)) {
  stop(simpleError(
    message = paste0(
      "Argument `", x_nm, "` was missing --- it has no default so ",
      "some value must be supplied!"
    ),
    call = call
  ))
}
