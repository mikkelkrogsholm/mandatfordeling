
<!-- README.md is generated from README.Rmd. Please edit that file -->
mandatfordeling
===============

Målet med mandatfordeling er at gøre det nemt at beregne mandatfordelinger ved Folketingsvalg i Danmark. Denne pakke udregner mandatfordelingen på storkredsniveau med både kredsmandater og tillægsmandater. Den skal bruge stemmetal på storkredsniveau for at regne det ud. Se eksempler i neden for

Installation
------------

Du kan installere mandatfordeling fra github således:

``` r
# install.packages("devtools")
devtools::install_github("mikkelkrogsholm/mandatfordeling")
```

Eksempel
--------

``` r
library(mandatfordeling)
```

``` r
# Pakken loader også data - lad os se på strukturen på vores input
dplyr::glimpse(storkreds_res)
#> Observations: 100
#> Variables: 3
#> $ storkreds <chr> "københavn", "københavns omegn", "nordsjælland", "bo...
#> $ parti     <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B...
#> $ stemmer   <int> 96753, 90361, 64822, 8683, 146464, 90061, 104391, 13...
```

``` r
# Lad os beregne mandatfordelingen
ft15 <- beregn_mandater(storkreds_res)

# Og se på output
dplyr::glimpse(ft15)
#> Observations: 90
#> Variables: 7
#> $ landsdel  <chr> "hovedstaden", "hovedstaden", "hovedstaden", "hoveds...
#> $ storkreds <chr> "københavn", "københavns omegn", "nordsjælland", "bo...
#> $ parti     <chr> "Å", "Å", "Å", "Å", "B", "B", "B", "B", "C", "C", "C...
#> $ votes     <int> 48475, 13575, 12957, 1299, 40578, 16710, 17515, 427,...
#> $ attained  <dbl> 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1...
#> $ extra     <dbl> 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0...
#> $ total     <dbl> 2, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 2, 1...
```
