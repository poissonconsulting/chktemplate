# Convert chk row to list of expressions

Convert chk row to list of expressions

## Usage

``` r
chkrow_to_expression(x)
```

## Arguments

- x:

  A data.frame with chk values for each column. Data.frame must have a
  column called 'name' and a value of 'chk' within that column
  corresponding to the row of chk values.

## Value

A named list of the chk values by column.
