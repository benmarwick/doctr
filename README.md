[![Build Status](https://travis-ci.org/ctlente/doctr.svg?branch=master)](https://travis-ci.org/ctlente/doctr)

# doctr

This is an R package to help check data consistency. In short, it attempts to automate as much as possible the task of verifying if everything is ok with a dataset.

`doctr` has 3 main functions: `examine`, `diagnose`, and `compare`. The first one generates an exploratory analysis of a table, the second one runs some user specified tests on the variables of a dataset and returns comprehensive reports with the results, while the last one compares two tables to see if they could be considered similar enough (usefull when we are analysing a dataset over time).

### Example

We can use `examine` to get a sense of how our dataset looks like

```r
X %>% examine() %>% summary_dbl()
#> # A tibble: 4 × 25
#>         name       min        max       `1%`      `5%`     `10%`     `20%`     `30%` `40%` `50%`   `60%`
#>        <chr>     <dbl>      <dbl>      <dbl>     <dbl>     <dbl>     <dbl>     <dbl> <dbl> <dbl>   <dbl>
#> 1   dinheiro    1.2300      12.23    1.23810    1.2705   1.31100   1.39200   1.47300   3.6  6.75  9.9000
#> 2   contagem    1.0000       6.00    1.05000    1.2500   1.50000   2.00000   2.50000   3.0  3.50  4.0000
#> 3 quantidade    1.1110 1234567.12    1.54656    3.2888   5.46660   9.82220  12.20000  12.6 13.00 13.0566
#> 4   continua -123.1234   12345.00 -119.48970 -104.9549 -86.78638 -50.44936 -14.11234  -1.6 -1.00 -0.4000
#> # ... with 14 more variables: `70%` <dbl>, `80%` <dbl>, `90%` <dbl>, `95%` <dbl>, `99%` <dbl>,
#> #   mean <dbl>, sd <dbl>, na <int>, val <int>, neg <int>, zero <int>, pos <int>, unq <int>, mdp <dbl>
```

Then we could set some tests to be run on the variables with `diagnose` (read the vignette for more information on how to set up these tests)

```r
X %>% diagnose(exams) %>% issues()
#> Issues found in 'dinheiro'
#>     More than 25% of entries are NAs
#> Issues found in 'contagem'
#>     4 entries are smaller than 5
#> Issues found in 'quantidade'
#>     1 entries are larger than 15
#> Issues found in 'continua'
#>     More than 0% of entries are NAs
#>     1 entries have more than 3 decimal places
#> Issues found in 'categorica'
#>     More than 0% of entries are NAs
#>     There are less than 5 unique classes
#>     There are more than 2 unique classes
#>     There are 2 classes that represent less than 50% of the total
```

Finally we could compare multiple versions of a table over time with the help of `compare`

```r
X_jan %>% compare(X_feb) %>% issues()
#> No issues found in 'dinheiro'
#> No issues found in 'contagem'
#> Issues found in 'quantidade'
#>     New value for 'na' is too low
#> Issues found in 'continua'
#>     New value for 'max' is too low
#>     New value for '30%' is too high
#>     New value for '40%' is too high
#>     New value for '50%' is too high
#>     New value for '60%' is too high
#>     New value for '70%' is too low
#>     New value for '80%' is too low
#>     New value for '90%' is too low
#>     New value for '95%' is too low
#>     New value for '99%' is too low
#>     New value for 'mean' is too low
#>     New value for 'sd' is too low
#>     New value for 'pos' is too low
#> No issues found in 'categorica'
```


