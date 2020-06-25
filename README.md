
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
  requireNamespace("easyassertions")
  dbc::assert_is_integer_ltezero_vector(x)
}


# function within a package
#' @importFrom dbc assert_is_integer_ltezero_vector
my_fun <- function(x) {
  dbc::assert_is_integer_ltezero_vector(x)
}
```

