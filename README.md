
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chktemplate

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/chktemplate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/chktemplate/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/chktemplate/branch/main/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/chktemplate?branch=main)
<!-- badges: end -->

Work with a Shiny templates by turning them into human-readable format
or confirm the data follows the template requirements.

## Installation

You can install the development version from
[GitHub](https://github.com/poissonconsulting/chktemplate) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/chktemplate")
```

## Demonstration

### Convert Template to be Human Readable

First you can see how the template is written which is in a code based
form, the code based form is then converted to be easily readable for
humans. We will subset to the first four columns since there are too
many columns to display on a single page.

``` r
library(chktemplate)
chktemplate::demo_template_fish_exploit$outing[1:4]
#>          name           outing_id            year           month
#> 1     example                   1            <NA>            <NA>
#> 2 description unique Id of outing  year of outing month of outing
#> 3         chk        c(0L, 1000L) c(2000L, 2099L)      c(1L, 12L)
#> 4        pkey                TRUE            <NA>            <NA>
#> 5      unique                TRUE            <NA>            <NA>
chktemplate::template_human(chktemplate::demo_template_fish_exploit$outing[1:4])
#> # A tibble: 5 × 4
#>   name            outing_id                  year                          month
#>   <chr>           <chr>                      <chr>                         <chr>
#> 1 description     unique Id of outing        year of outing                mont…
#> 2 example         1                          2024                          3    
#> 3 constraint      integer between 0 and 1000 integer between 2000 and 2099 inte…
#> 4 missing_allowed no                         no                            no   
#> 5 unique          yes                        no                            no
```

### Check Data Against Template Requirements

## Code of Conduct

Please note that the `chktemplate` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
