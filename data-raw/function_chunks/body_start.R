# @codedoc_comment_block news("dbc", "2023-08-16", "0.4.16")
# All generated assertion, report and test functions now check whether
# `x` is missing and raise an informative error. There were edge cases
# where `x` was attempted to be evaluated only in a call to `eval` which
# resulted in cryptic error messages --- now those can no longer occur.
# @codedoc_comment_block news("dbc", "2023-08-16", "0.4.16")
dbc::handle_args_inplace()
