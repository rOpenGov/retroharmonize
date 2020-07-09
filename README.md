
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retroharmonize

<!-- badges: start -->

[![Project Status: Minimal or no implementation has been done yet, or
the repository is only intended to be a limited example, demo, or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
“[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)”
“[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/eurobarometer)](https://cran.r-project.org/package=eurobarometer)”
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)

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

## Example

``` r
library(retroharmonize)
library(haven)
#> 
#> Attaching package: 'haven'
#> The following object is masked from 'package:retroharmonize':
#> 
#>     read_spss

v2 <- haven::labelled_spss (c(1,1,0,8), 
                            labels = c("yes" = 1,
                                       "no"  = 0,
                                       "declined" = 8),
                            na_values = 8)

h2 <- harmonize_values(v2, 
                       harmonize_labels = list(
                         from = c("^yes", "^no", "^inap"), 
                         to = c("trust", "not_trust", "inap"), 
                         numeric_values = c(1,0,99999)), 
                       id = 'survey2' )
str(h2)
#>  dbl+lbl [1:4]     1,     1,     0, 99901
#>  @ labels                 : Named num [1:3] 0 1 99901
#>   ..- attr(*, "names")= chr [1:3] "not_trust" "trust" "invalid_label"
#>  @ na_values              : num 99901
#>  @ survey2_labels_orig    : Named num [1:3] 1 0 99901
#>   ..- attr(*, "names")= chr [1:3] "yes" "no" ""
#>  @ survey2_original_values: Named num [1:3] 1 0 99901
#>   ..- attr(*, "names")= chr [1:3] "1" "0" "8"
```

## Code of Conduct

Please note that the surveyharmonize project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
