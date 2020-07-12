
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
#> The following object is masked from 'package:retroharmonize':
#> 
#>     read_spss

h2 <- harmonize_values(
  eb$qa14_2,  
  harmonize_labels = list(
    from = c("^tend to", "^tend not", "dk", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"), 
    numeric_values = c(1,0,99997,99999)
    ), 
  id = attr(eb, "id") )
str(h2)
#>  dbl+lbl [1:45]     0,     1,     1, 99997,     1,     1,     1, 99997, 999...
#>  @ labels                             : Named num [1:4] 0 1 99997 99999
#>   ..- attr(*, "names")= chr [1:4] "not_trust" "trust" "do_not_know" "inap"
#>  @ na_values                          : num [1:2] 99997 99999
#>  @ Eurobarometer_91_5_subsample_labels: Named num [1:4] 1 2 3 9
#>   ..- attr(*, "names")= chr [1:4] "Tend to trust" "Tend not to trust" "DK" "Inap. (not CY-TCC in isocntry and not 1 in eu28)"
#>  @ Eurobarometer_91_5_subsample_values: Named num [1:4] 0 1 99997 99999
#>   ..- attr(*, "names")= chr [1:4] "2" "1" "3" "9"
#>  @ id                                 : chr "Eurobarometer_91_5_subsample"
```

``` r
#the label ordering is not yet harmonized!!!
tibble::tibble (
  values  = labelled::val_labels(h2), 
  labels = names(labelled::val_labels(h2)),
  label_orig = names(attr(h2, "Eurobarometer_91_5_subsample_labels")),
  values_orig = names(attr(h2, "Eurobarometer_91_5_subsample_values"))
)
#> # A tibble: 4 x 4
#>   values labels      label_orig                                      values_orig
#>    <dbl> <chr>       <chr>                                           <chr>      
#> 1      0 not_trust   Tend to trust                                   2          
#> 2      1 trust       Tend not to trust                               1          
#> 3  99997 do_not_know DK                                              3          
#> 4  99999 inap        Inap. (not CY-TCC in isocntry and not 1 in eu2~ 9
```

## Code of Conduct

Please note that the surveyharmonize project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
