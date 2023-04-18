
<!-- generated by R package codedoc; do not modify! -->

# Package `dbc`


`dbc` is designed to aid writing functions under the design by contract
philosophy, where function inputs and outputs are programmatically
asserted to adhere to specifications.

<!-- badges: start -->
[![R-CMD-check](https://github.com/WetRobot/dbc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WetRobot/dbc/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/FinnishCancerRegistry/dbc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/dbc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Recommended installation

```r
devtools::install_github(
"FinnishCancerRegistry/dbc",
ref = readline("enter latest tag on github: ")
)
```

# Example
```r
# by adding arg assertion_type, you can use the same function for end-user
# purposes and internal purposes with clear error messages.
my_fun <- function(df, by, assertion_type = NULL) {
if (is.null(assertion_type)) {
assertion_type <- dbc::assertion_type_default()
}
dbc::assert_is_character_nonNA_vector(
x = by,
assertion_type = assertion_type
)
dbc::assert_is_data.frame_with_required_names(
x = df,
required_names = by,
assertion_type = assertion_type
)
return(table(df[,by]))
}
my_fun(df, c("var_1", "var_2"))
my_fun_2 <- function(df) {
my_fun(df, c("var_1", "var_2"), assertion_type = "prod_input")
}
```



