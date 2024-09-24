
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chktemplate

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/chktemplate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/chktemplate/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/chktemplate/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/chktemplate)
<!-- badges: end -->

Work with data template by turning them into a human-readable format and
confirm the data follows all the requirements.

## Installation

You can install the development version from
[GitHub](https://github.com/poissonconsulting/chktemplate) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/chktemplate")
```

## Demonstration

``` r
library(chktemplate)
```

### Convert Template to be Human Readable

The templates need to be written in code but this is not very readable
for a user. Use the `template_human()` function to convert the template
into a human readable form.

``` r
# subset the first four columns so example fits on page 
# code version of the template
demo_template_fish_exploit$outing[1:4]
#>          name           outing_id        site_name            year
#> 1     example                   1        Bendy Bay            2015
#> 2 description unique Id of outing name of the site  year of outing
#> 3         chk         c(0L, 100L)            c("") c(2000L, 2099L)
#> 4        pkey                TRUE             <NA>            <NA>
#> 5      unique                TRUE             <NA>            <NA>
#> 6       join1                <NA>             site            <NA>
# human readable version of the template
template_human(demo_template_fish_exploit$outing[1:4])
#> # A tibble: 7 × 4
#>   name            outing_id                 site_name        year               
#>   <chr>           <chr>                     <chr>            <chr>              
#> 1 description     unique Id of outing       name of the site year of outing     
#> 2 example         1                         Bendy Bay        2015               
#> 3 constraint      integer between 0 and 100 any word(s)      integer between 20…
#> 4 missing allowed no                        no               no                 
#> 5 primary key     yes                       no               no                 
#> 6 unique          yes                       no               no                 
#> 7 joins to        <NA>                      site             <NA>
```

### Check Data Against Template Requirements

Pass data and the template to `check_data_format()` to ensure the data
follows all the rules and requirements of the template. It can check
whether all columns are supplied, the type of the column, the range of
values, the primary key, uniqueness of the column, and joins between
tables.

``` r
outing <- data.frame(
  outing_id = c(1L, 2L, 3L),
  site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
  year = c(2010, 2010, 2010),
  month = c(07, 07, 07),
  day = c(15, 16, 17),
  hour_start = c(9L, 11L, 8L),
  minute_start = c(0, 0, 0),
  guide = c("JT", "JT", "JT"),
  rod_count = c(2, 3, 2),
  comment = c(NA_character_, NA_character_, NA_character_)
)

data <- check_data_format(
  outing = outing,
  template = demo_template_fish_exploit
)
```

The `complete` argument checks if all tables are present.

``` r
site <- data.frame(
    site_name = c("Pretty Bay", "Ugly Bay", "Green Bay")
  )

outing <- data.frame(
  outing_id = c(1L, 2L, 3L),
  site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
  year = c(2010, 2010, 2010),
  month = c(7, 7, 7),
  day = c(15, 16, 17),
  hour_start = c(9L, 11L, 8L),
  minute_start = c(0, 0, 0),
  guide = c("JT", "JT", "JT"),
  rod_count = c(2, 3, 2),
  comment = c(NA_character_, NA_character_, NA_character_)
)

capture <- data.frame(
  outing_id = c(1L, 2L, 3L),
  guide = c("JT", "JT", "JT"),
  hour = c(7L, 8L, 7L),
  minute = c(0L, 30L, 45L),
  easting = c(1031941, 1031971, 1031944),
  northing = c(892421, 892451, 892429),
  species = c("BT", "CT", "CT"),
  forklength_mm = c(100, 700, 300),
  weight_kg = c(0.5, 10, 4),
  tbartag_number1 = c(78, 91, 82),
  tbartag_number2 = c(14, 18, 21),
  released = c("yes", "no", "no")
)

recapture <- data.frame(
  year = c(2009, 2009),
  month = c(10, 10),
  day = c(14, 15),
  angler = c("Dave John", "John Smith"),
  contact = c("250-637-9999", "250-557-1414"),
  tbartag_number1 = c(92, 57),
  tbartag_number2 = c(10, 12)
)

data <- check_data_format(
  site = site,
  outing = outing,
  capture = capture,
  recapture = recapture,
  template = demo_template_fish_exploit,
  complete = TRUE
)
```

Joins are only checked if `join` is set to `TRUE`.

``` r
site <- data.frame(
    site_name = c("Pretty Bay", "Ugly Bay", "Green Bay")
  )

outing <- data.frame(
  outing_id = c(1L, 2L, 3L),
  site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
  year = c(2010, 2010, 2010),
  month = c(7, 7, 7),
  day = c(15, 16, 17),
  hour_start = c(9L, 11L, 8L),
  minute_start = c(0, 0, 0),
  guide = c("JT", "JT", "JT"),
  rod_count = c(2, 3, 2),
  comment = c(NA_character_, NA_character_, NA_character_)
)

capture <- data.frame(
  outing_id = c(1L, 2L, 3L),
  guide = c("JT", "JT", "JT"),
  hour = c(7L, 8L, 7L),
  minute = c(0L, 30L, 45L),
  easting = c(1031941, 1031971, 1031944),
  northing = c(892421, 892451, 892429),
  species = c("BT", "CT", "CT"),
  forklength_mm = c(100, 700, 300),
  weight_kg = c(0.5, 10, 4),
  tbartag_number1 = c(78, 91, 82),
  tbartag_number2 = c(14, 18, 21),
  released = c("yes", "no", "no")
)

recapture <- data.frame(
  year = c(2009, 2009),
  month = c(10, 10),
  day = c(14, 15),
  angler = c("Dave John", "John Smith"),
  contact = c("250-637-9999", "250-557-1414"),
  tbartag_number1 = c(92, 57),
  tbartag_number2 = c(10, 12)
)

data <- check_data_format(
  site = site,
  outing = outing,
  capture = capture,
  recapture = recapture,
  template = demo_template_fish_exploit,
  complete = TRUE,
  join = TRUE
)
```

## Template Requirements

In the templates the first column needs to be the name column which is a
special column. The name column sets out the rules for the template. No
other column can be called name.

Description of each row:

- name: column names
- example: sample value, optional
- description: written description of the column
- chk: range, type, missing value and set checks, see
  [chk::chk_values](https://poissonconsulting.github.io/chk/reference/check_values.html)
  for how values are checked
- pkey: primary key, set values as `TRUE`, can be a combination of
  columns
- unique: column has to be unique, set values as `TRUE`, optional
- join1: place parent table name in the variable(s) that should be the
  `by` argument of join, optional
  - a join row can only join to one table, to join to multiple tables
    add more join rows like join2, join3, etc.
  - Check `chktemplate::demo_template_count` template for an example of
    a table with multiple join rows.

## Code of Conduct

Please note that the `chktemplate` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
