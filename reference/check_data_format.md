# Check Data follow Required Format as per the Template

Check the types, ranges, sets of values, primary keys, missing values,
unique and join requirements of the template against the data to ensure
the data follows the format outlined in the template.

## Usage

``` r
check_data_format(..., template, complete = FALSE, join = FALSE)
```

## Arguments

- ...:

  A list of named data frames of the data

- template:

  A list of named data frames that make up the template

- complete:

  A logical indicating if all tables present in the template need to be
  supplied in the ... argument. If TRUE all tables need to be provided
  in ... argument, default is set to FALSE.

- join:

  A logical indicating if joins between the tables should be checked. If
  TRUE joins are checked, default is set to FALSE.

## Value

A list of named data frames of the data

## Details

The template argument should contain all the sheets in the template
while the ... argument can be which ever set of data tables you want to
check against the template. This means either one, several or all the
data tables can be checked against the template.

If complete is set to TRUE then all the data tables need to be supplied.
If join is set to TRUE then the joins between the provided tables are
checked.

## Examples

``` r
if (FALSE) { # \dontrun{
check_data_format(
  outing = outing,
  template = demo_template_fish_exploit,
  complete = FALSE,
  join = FALSE
)

check_data_format(
  site = site,
  outing = outing,
  capture = capture,
  recapture = recapture,
  template = demo_template_fish_exploit,
  complete = TRUE,
  join = TRUE
)
} # }
```
