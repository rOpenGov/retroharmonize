
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retroharmonize

<!-- badges: start -->

[![Project Status: Minimal or no implementation has been done yet, or
the repository is only intended to be a limited example, demo, or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/eurobarometer)](https://cran.r-project.org/package=eurobarometer)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3937746.svg)](https://doi.org/10.5281/zenodo.3937746)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
[![Codecov test
coverage](https://codecov.io/gh/antaldaniel/retroharmonize/branch/master/graph/badge.svg)](https://codecov.io/gh/antaldaniel/retroharmonize?branch=master)
[![Travis build
status](https://travis-ci.com/antaldaniel/retroharmonize.svg?branch=master)](https://travis-ci.com/antaldaniel/retroharmonize)
<!-- badges: end -->

The goal of `retroharmonize` is to allow the organization of data joins
or panels from various data sources, particularly survey microdata
files, by *retrospective harmonization* the value codes, the value
labels, and the missing value ranges of the data in a reproducible
manner with the help of comprehensive s3 classes.

Currently being generalized from problems solved in the
[github.com/antaldaniel/eurobarometer](\(https://github.com/antaldaniel/eurobarometer\))
package ([doi](https://doi.org/10.5281/zenodo.3825700).)

You can download the manual in [PDF](retroharmonize_0.1.1.pdf).

## Installation

Soon you can install the released version of retroharmonize from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("retroharmonize")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("antaldaniel/retroharmonize")
```

## Classes for retrospective harmonization

``` r
library(retroharmonize)
eb <- read_rds ( 
  file = system.file(
    "examples", "ZA7576.rds", package = "retroharmonize"), 
  id = "Eurobarometer_91_5_subsample", 
  doi = "10.4232/1.13393"
  )
print(eb)
#> # A tibble: 45 x 56
#>    rowid doi   version uniqid caseid serialid isocntry       p1      p2    p3
#>  * <chr> <chr> <chr>    <dbl>  <dbl>    <dbl> <chr>    <dbl+lb> <dbl+l> <dbl>
#>  1 Euro~ doi:~ 1.0.0 ~ 5.00e7    481     3209 ES        4 [Mon~ 3 [13 ~    25
#>  2 Euro~ doi:~ 1.0.0 ~ 1.10e8     76     8706 NL        6 [Wed~ 3 [13 ~    58
#>  3 Euro~ doi:~ 1.0.0 ~ 1.10e8    343     8890 NL       11 [Mon~ 3 [13 ~    56
#>  4 Euro~ doi:~ 1.0.0 ~ 1.10e8    473     8989 NL        5 [Tue~ 3 [13 ~    62
#>  5 Euro~ doi:~ 1.0.0 ~ 1.10e8    493     9001 NL        8 [Fri~ 4 [17 ~    30
#>  6 Euro~ doi:~ 1.0.0 ~ 1.10e8    897     9272 NL        6 [Wed~ 3 [13 ~    56
#>  7 Euro~ doi:~ 1.0.0 ~ 1.10e8   1041     9379 NL        5 [Tue~ 3 [13 ~    57
#>  8 Euro~ doi:~ 1.0.0 ~ 1.10e8   1192     9493 NL        6 [Wed~ 2 [8 -~    60
#>  9 Euro~ doi:~ 1.0.0 ~ 1.10e8   1274     9543 NL        7 [Thu~ 4 [17 ~    57
#> 10 Euro~ doi:~ 1.0.0 ~ 1.10e8   1344     9590 NL        6 [Wed~ 2 [8 -~    83
#> # ... with 35 more rows, and 46 more variables: p4 <dbl+lbl>, p5 <dbl+lbl>,
#> #   nuts <chr+lbl>, d7 <dbl+lbl>, d8 <dbl+lbl>, d25 <dbl+lbl>, d60 <dbl+lbl>,
#> #   qa14_5 <dbl+lbl>, qa14_3 <dbl+lbl>, qa14_2 <dbl+lbl>, qa14_4 <dbl+lbl>,
#> #   qa14_1 <dbl+lbl>, qa6a_5 <dbl+lbl>, qa6a_10 <dbl+lbl>, qa6b_2 <dbl+lbl>,
#> #   qa6a_3 <dbl+lbl>, qa6a_1 <dbl+lbl>, qa6b_4 <dbl+lbl>, qa6a_8 <dbl+lbl>,
#> #   qa6a_9 <dbl+lbl>, qa6a_4 <dbl+lbl>, qa6a_2 <dbl+lbl>, qa6b_1 <dbl+lbl>,
#> #   qa6a_6 <dbl+lbl>, qa6a_7 <dbl+lbl>, qa6a_11 <dbl+lbl>, qa6b_3 <dbl+lbl>,
#> #   qd6.1 <dbl+lbl>, qd6.2 <dbl+lbl>, qd6.3 <dbl+lbl>, qd6.4 <dbl+lbl>,
#> #   qd6.5 <dbl+lbl>, qd6.6 <dbl+lbl>, qd6.7 <dbl+lbl>, qd6.8 <dbl+lbl>,
#> #   qd6.9 <dbl+lbl>, qd6.10 <dbl+lbl>, qd6.11 <dbl+lbl>, qd6.12 <dbl+lbl>,
#> #   qd6.13 <dbl+lbl>, qd6.14 <dbl+lbl>, qg1b <dbl+lbl>, qg8 <dbl+lbl>,
#> #   w1 <dbl>, w3 <dbl>, wex <dbl>
```

The `labelled_spss_survey` class is an extension of haven’s
`labelled_spss` class. It not only preserver variable and value labels
and the user-defined missing range, but also gives an identifer, for
example, the filename to the vector. See [Working With The
labelled\_spss\_survey
Class](http://retroharmonize.satellitereport.com/articles/labelled_spss_survey.html).

## Harmonize categorical variables and missing values

The aim of `retroharmonize` is to help the reproducible harmonization of
survey data. This means that not only the numeric codes, the labels and
the missing range is harmonized, but the original coding is preserved as
metadata attributes for documentation and validation purposes. See more
in the article [Harmonize Value
Labels](http://retroharmonize.satellitereport.com/articles/harmonize_labels.html).

``` r
library(retroharmonize)
library(haven)
#> 
#> Attaching package: 'haven'
#> The following objects are masked from 'package:retroharmonize':
#> 
#>     as_factor, read_spss

h2 <- harmonize_values(
  eb$qa14_2,  
  harmonize_labels = list(
    from = c("^tend to", "^tend not", "dk", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"), 
    numeric_values = c(1,0,99997,99999)
    ), 
  id = attr(eb, "id") )

h2_documentation <- document_survey_item(h2)
```

``` r
h2_documentation$code_table
#> # A tibble: 4 x 5
#>   values Eurobarometer_91_5_subs~ labels   Eurobarometer_91_5_subsample~ missing
#>    <dbl>                    <dbl> <chr>    <chr>                         <lgl>  
#> 1      0                        1 not_tru~ Tend to trust                 FALSE  
#> 2      1                        2 trust    Tend not to trust             FALSE  
#> 3  99997                        3 do_not_~ DK                            TRUE   
#> 4  99999                        9 inap     Inap. (not CY-TCC in isocntr~ TRUE
```

``` r
h2_documentation$history_var_name
#>                              name Eurobarometer_91_5_subsample_name 
#>                              "h2"                       "eb$qa14_2"
```

``` r
h2_documentation$history_var_label
#>                              label Eurobarometer_91_5_subsample_label 
#>      "EUROPEAN COMMISSION - TRUST"      "EUROPEAN COMMISSION - TRUST"
```

## Create a longitudional table

``` r
var1 <- labelled::labelled_spss(
  x = c(1,0,1,1,0,8,9), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0, 
             "DON'T KNOW" = 8, 
             "INAP. HERE" = 9), 
  na_values = c(8,9))

var2 <- labelled::labelled_spss(
  x = c(2,2,8,9,1,1 ), 
  labels = c("Tend to trust" = 1, 
             "Tend not to trust" = 2, 
             "DK" = 8, 
             "Inap" = 9), 
  na_values = c(8,9))


h1 <- harmonize_values (
  x = var1, 
  harmonize_label = "Do you trust the European Union?",
  harmonize_labels = list ( 
    from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"),
    numeric_values = c(1,0,99997, 99999)), 
  na_values = c("do_not_know" = 99997,
                "inap" = 99999), 
  id = "survey1",

)

h2 <- harmonize_values (
  x = var2, 
  harmonize_label = "Do you trust the European Union?",
  harmonize_labels = list ( 
    from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"),
    numeric_values = c(1,0,99997, 99999)), 
  na_values = c("do_not_know" = 99997,
                "inap" = 99999), 
  id = "survey2"
)

a <- tibble::tibble ( rowid = paste0("survey1", 1:length(h1)),
                      hvar = h1, 
                      w = runif(n = length(h1), 0,1))
b <- tibble::tibble ( rowid = paste0("survey2", 1:length(h2)),
                      hvar  = h2, 
                      w = runif(n = length(h2), 0,1))

dplyr::bind_rows(a,b)
#> # A tibble: 13 x 3
#>    rowid                        hvar      w
#>    <chr>                <retroh_dbl>  <dbl>
#>  1 survey11     1 [trust]            0.0802
#>  2 survey12     0 [not_trust]        0.719 
#>  3 survey13     1 [trust]            0.935 
#>  4 survey14     1 [trust]            0.825 
#>  5 survey15     0 [not_trust]        0.426 
#>  6 survey16 99997 (NA) [do_not_know] 0.587 
#>  7 survey17 99999 (NA) [inap]        0.291 
#>  8 survey21     0 [not_trust]        0.142 
#>  9 survey22     0 [not_trust]        0.181 
#> 10 survey23 99997 (NA) [do_not_know] 0.477 
#> 11 survey24 99999 (NA) [inap]        0.997 
#> 12 survey25     1 [trust]            0.349 
#> 13 survey26     1 [trust]            0.297
```

See the [Case Study: Working With
Afrobarometer](articles/harmonize_labels.html) for a futher automated
workflow.

## Code of Conduct

Please note that the retroharmonize project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
