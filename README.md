
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chktemplate

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/poissonconsulting/chktemplate/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/chktemplate/actions)
[![codecov](https://codecov.io/gh/poissonconsulting/chktemplate/branch/master/graph/badge.svg?token=FR6YQNTZF3)](https://codecov.io/gh/poissonconsulting/chktemplate)
<!-- badges: end -->

Modify a `shinyupload` template to human-readable format.

## Installation

You can install the development version from
[GitHub](https://github.com/poissonconsulting/chktemplate) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/chktemplate")
```

## Demonstration

`chktemplate` is an R package of utility functions. It is designed to be
used in conjunction with the `shinyupload2` package.

Consider the demonstration template - each data frame is converted into
human readable format using the `template_human()` function.

``` r
library(chktemplate)
library(tibble)
chktemplate::demo_template$outing
#> # A tibble: 5 x 18
#>   name  outing_id year  month day   hour_start minute_start hour_end minute_end
#>   <chr> <chr>     <chr> <chr> <chr> <chr>      <chr>        <chr>    <chr>     
#> 1 exam… 1         <NA>  <NA>  <NA>  <NA>       <NA>         <NA>     <NA>      
#> 2 desc… unique I… year… mont… day … hour of o… minute of o… hour of… minute of…
#> 3 chk   c(0L, 10… c(20… c(1L… c(1L… c(0L, 23L) c(0L, 59L)   c(0L, 2… c(0L, 59L)
#> 4 pkey  TRUE      <NA>  <NA>  <NA>  <NA>       <NA>         <NA>     <NA>      
#> 5 uniq… TRUE      <NA>  <NA>  <NA>  <NA>       <NA>         <NA>     <NA>      
#> # … with 9 more variables: boat <chr>, guide <chr>, crew1 <chr>, crew2 <chr>,
#> #   crew_gps <chr>, crew_camera <chr>, watertemp_degc <chr>, rod_count <chr>,
#> #   comment <chr>
set.seed(42)
chktemplate::template_human(chktemplate::demo_template$outing)
#> # A tibble: 5 x 18
#>   name  outing_id year  month day   hour_start minute_start hour_end minute_end
#>   <chr> <chr>     <chr> <chr> <chr> <chr>      <chr>        <chr>    <chr>     
#> 1 desc… unique I… year… mont… day … hour of o… minute of o… hour of… minute of…
#> 2 exam… 1         2064  9     10    3          17           16       46        
#> 3 cons… integer … inte… inte… inte… integer b… integer bet… integer… integer b…
#> 4 miss… no        no    no    no    no         no           no       no        
#> 5 uniq… yes       no    no    no    no         no           no       no        
#> # … with 9 more variables: boat <chr>, guide <chr>, crew1 <chr>, crew2 <chr>,
#> #   crew_gps <chr>, crew_camera <chr>, watertemp_degc <chr>, rod_count <chr>,
#> #   comment <chr>
```

## Code of Conduct

Please note that the `chktemplate` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
