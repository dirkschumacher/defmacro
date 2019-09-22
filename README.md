
<!-- README.md is generated from README.Rmd. Please edit that file -->

# defmacro

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `defmacro` is to experiment with compile time macros in R.
The idea is to add a macro expansion step during the `.onLoad` step of
the package.

A macro is a function that takes code and returns code.

An example package is
[here](https://github.com/dirkschumacher/defmacroex).

## Example

For example in a package you define a macro that evaluates an expression
at “compile time”:

``` r
constexpr <- defmacro::defmacro(function(expr) {
  eval(expr)
})
```

Then you might have a regular function that tests if a value exceeds a
quantile of the standard normal distribtion:

``` r
is_invalid <- function(value) {
  value > constexpr(qnorm(0.975))
}
```

After macro expansion during `.onLoad` the following function is
exported to the user:

``` r
defmacro::expand_function(is_invalid)
#> function (value) 
#> {
#>     value > 1.95996398454005
#> }
```

Thus the call to `qnorm` never happens at runtime as it could have been
evaluated during package load.

You could also define your own piping function and have all the overhead
removed during runtime:

``` r
`%>%` <- defmacro::defmacro(function(lhs, rhs) {
  fun_args <- c(list(lhs), unlist(as.list(rhs[-1L]), FALSE))
  rlang::get_expr(rlang::quo(`!!`(rhs[[1L]])(!!!(fun_args))))
})
```

``` r
analyze_dataset <- function(data) {
  data %>%
    dplyr::filter(hp > constexpr(50 + 50)) %>%
    dplyr::group_by(cyl) %>%
    dplyr::summarise(dplyr::n())
}
```

``` r
defmacro::expand_function(analyze_dataset)
#> function (data) 
#> {
#>     dplyr::summarise(dplyr::group_by(dplyr::filter(data, hp > 
#>         100), cyl), dplyr::n())
#> }
```
