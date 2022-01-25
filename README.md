
# dbc

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/WetRobot/easyassertions.svg?branch=master)](https://travis-ci.org/WetRobot/dbc)
<!-- badges: end -->

dbc assists in design-by-contract development of R functions. 
It also enables you to write simple, one-line and one-function-call assertions.

## Installation

``` r
devtools::install_github("WetRobot/dbc")
```

## Example

You would probably use this package to write assertions for your own functions
and possibly your own package. I recommend the following style of using
dbc functions:

``` r
# function outside a packge
my_fun <- function(x) {
  requireNamespace("dbc")
  dbc::assert_is_integer_ltezero_vector(x)
  x + 1
}


# function within a package
my_fun <- function(x) {
  dbc::assert_is_integer_ltezero_vector(x)
  x + 1
}

# want to distinguish functions intended for user and intended for use
# in other functions only (or reuse the same code for some other reason)
my_fun__ <- function(x, y, assertion_type, call = NULL) {
  call <- dbc::handle_arg_call(call)
  dbc::report_is_integer_ltezero_vector(x)
  dbc::assert_is_character_nonNA_atom(y)
  switch(
    y,
    add = x + 1,
    subtract = x - 1
  )
}
my_fun_ <- function(x, y) {
  call <- dbc::handle_arg_call(NULL)
  my_fun__(x, y, "prod_input", call)
}
my_fun <- function(x, y) {
  call <- dbc::handle_arg_call(NULL)
  my_fun__(x, y, "user_input", call)
}


```

