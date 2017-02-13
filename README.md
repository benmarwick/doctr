[![Build Status](https://travis-ci.org/ctlente/doctr.svg?branch=master)](https://travis-ci.org/ctlente/doctr)

# doctr

This is an R package to help check data consistency. In short, it attempts to automate as much as possible the task of verifying if everything is ok with a dataset.

`doctr` has 3 main functions: `examine`, `diagnose`, and `compare`. The first one generates an exploratory analysis of a table, the second one runs some user specified tests on the variables of a dataset and returns comprehensive reports with the results, while the last one compares two tables to see if they could be considered similar enough (usefull when we are analysing a dataset over time).

### Example

We can use `examine` with `report_*` to get a sense of what our dataset looks like

```r
X %>% examine() %>% report_num()
#> # A tibble: 14 × 26
#>                      name    len   min         max     `1%`         `5%`   `10%`
#>                     <chr>  <int> <dbl>       <dbl>    <dbl>        <dbl>   <dbl>
#> 1                  numdbm 500000   122 90300832.00 610696.3 3584609.0000 8149172
#> 2    qtd_compras_dezembro 500000     0        8.00      0.0       0.0000       0
#> 3    vlr_compras_dezembro 500000     0   140534.50      0.0       0.0000       0
#> 4    qtd_compras_sazonais 500000     0       60.00      0.0       0.0000       0
#> 5    vlr_compras_sazonais 500000     0    78912.23     29.0      59.0000      88
#> 6        qtd_compras_vida 500000     1      531.00      1.0       1.0000       1
#> 7        vlr_compras_vida 500000     1   881556.41     39.9      84.6995     139
#> 8        qtd_compras_loja 500000     1      182.00      1.0       1.0000       1
#> 9      valor_compras_loja 500000     1   228722.85     38.0      69.0000      99
#> 10    tempo_ultima_compra 500000     0       23.00      0.0       0.0000       1
#> 11   media_itens_cesta_t2 500000     0      164.00      0.0       0.0000       0
#> 12   ticket_medio_loja_t2 500000     0    11939.43      0.0       0.0000       0
#> 13 vlr_comproufestacartao 500000     0     6013.73      0.0       0.0000       0
#> 14 media_itens_cesta_loja 500000     1      144.00      1.0       1.0000       1
#> # ... with 19 more variables: `20%` <dbl>, `30%` <dbl>, `40%` <dbl>, `50%` <dbl>,
#> #   `60%` <dbl>, `70%` <dbl>, `80%` <dbl>, `90%` <dbl>, `95%` <dbl>, `99%` <dbl>,
#> #   mean <dbl>, sd <dbl>, na <dbl>, val <dbl>, neg <dbl>, zero <dbl>, pos <dbl>,
#> #   unq <int>, mdp <dbl>
```

Then we could set some tests to be run on the variables with `diagnose` and see if they pass with `issues` (the aforementioned tests can be created manually or with the help of `guess_exams`)

```r
X %>% diagnose(exams) %>% issues()
#> No issues found in 'numdbm'
#> Issues found in 'qtd_compras_dezembro'
#>     368 entries are larger than 5
#> Issues found in 'vlr_compras_dezembro'
#>     1 entries are larger than 1e+05
#> No issues found in 'qtd_compras_sazonais'
#> No issues found in 'vlr_compras_sazonais'
#> No issues found in 'qtd_compras_vida'
#> No issues found in 'vlr_compras_vida'
#> No issues found in 'qtd_compras_loja'
#> No issues found in 'valor_compras_loja'
#> No issues found in 'tempo_ultima_compra'
#> No issues found in 'resposta_t2'
#> No issues found in 'media_itens_cesta_t2'
#> No issues found in 'ticket_medio_loja_t2'
#> Issues found in 'primeira_compra_dezembro'
#>     Data isn't of type character
#> No issues found in 'flag_abordadofesta'
#> No issues found in 'flag_comproufestacartao'
#> No issues found in 'vlr_comproufestacartao'
#> No issues found in 'media_itens_cesta_loja'
```

Finally we could compare multiple versions of a table over time with the help of `compare` and `issues()`

```r
X_jan %>% compare(X_feb) %>% issues()
#> Issues found in 'numdbm'
#>     New value for 'max' is too high
#>     New value for '20%' is too low
#> No issues found in 'qtd_compras_dezembro'
#> Issues found in 'vlr_compras_dezembro'
#>     New value for 'max' is too high
#> Issues found in 'qtd_compras_sazonais'
#>     New value for 'sd' is too low
#> Issues found in 'vlr_compras_sazonais'
#>     New value for 'max' is too high
#>     New value for '95%' is too high
#>     New value for 'mean' is too high
#>     New value for 'sd' is too high
#> No issues found in 'qtd_compras_vida'
#> No issues found in 'vlr_compras_vida'
#> No issues found in 'qtd_compras_loja'
#> Issues found in 'valor_compras_loja'
#>     New value for 'max' is too high
#>     New value for 'sd' is too high
#> Issues found in 'tempo_ultima_compra'
#>     New value for 'zero' is too low
#>     New value for 'pos' is too high
#> No issues found in 'resposta_t2'
#> No issues found in 'media_itens_cesta_t2'
#> Issues found in 'ticket_medio_loja_t2'
#>     New value for 'max' is too high
#> No issues found in 'primeira_compra_dezembro'
#> No issues found in 'flag_abordadofesta'
#> No issues found in 'flag_comproufestacartao'
#> No issues found in 'vlr_comproufestacartao'
#> Issues found in 'media_itens_cesta_loja'
#>     New value for 'max' is too highow
```


