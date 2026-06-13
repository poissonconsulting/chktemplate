# Create Human Readable Template

Convert code written template to a human readable version.

## Usage

``` r
template_human(template)
```

## Arguments

- template:

  A data.frame of the template. with row names including 'description',
  'chk', 'unique' and optionally, 'example'.

## Value

The modified data.frame.

## Details

The template must contain a column called 'name'. The name column
includes rows named 'description', 'chk', 'unique' and optionally,
'example', 'unique' and 'join'.

## Examples

``` r
template_human(demo_template_count$visit)
#> # A tibble: 7 × 5
#>   name            year                          month                day   site 
#>   <chr>           <chr>                         <chr>                <chr> <chr>
#> 1 description     year of visit                 month of visit       day … "nam…
#> 2 example         2010                          10                   23    ""   
#> 3 constraint      integer between 2000 and 2099 integer between 1 a… inte… "any…
#> 4 missing allowed no                            no                   no    "no" 
#> 5 primary key     yes                           yes                  yes   "yes"
#> 6 unique          no                            no                   no    "no" 
#> 7 joins to        NA                            NA                   NA     NA  
template_human(demo_template_fish_exploit$capture)
#> # A tibble: 7 × 13
#>   name       outing_id guide hour  minute easting northing species forklength_mm
#>   <chr>      <chr>     <chr> <chr> <chr>  <chr>   <chr>    <chr>   <chr>        
#> 1 descripti… outing i… init… hour… minut… eastin… northin… specie… fork length …
#> 2 example    1         GA    10    42     523012  5497670  BT      510          
#> 3 constraint integer … any … inte… integ… intege… integer… any wo… integer betw…
#> 4 missing a… no        no    no    no     no      no       no      no           
#> 5 primary k… yes       yes   yes   yes    no      no       no      no           
#> 6 unique     no        no    no    no     no      no       no      no           
#> 7 joins to   outing    NA    NA    NA     NA      NA       NA      NA           
#> # ℹ 4 more variables: weight_kg <chr>, tbartag_number1 <chr>,
#> #   tbartag_number2 <chr>, released <chr>
```
