
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveyharmonize

<!-- badges: start -->

[![Project Status: Minimal or no implementation has been done yet, or
the repository is only intended to be a limited example, demo, or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
“[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)”
“[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/eurobarometer)](https://cran.r-project.org/package=eurobarometer)”
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)

<!-- badges: end -->

The goal of `surveyharmonize` is to allow the organization of data joins
or panels from various data sources, particularly survey microdata
files, by *retrospective harmonization* the value codes, the value
labels, and the missing value ranges of the data in a reproducible
manner with the help of comprehensive s3 classes.

Currently being generalized from problems solved in the
[github.com/antaldaniel/eurobarometer](\(https://github.com/antaldaniel/eurobarometer\))
package ([doi](https://doi.org/10.5281/zenodo.3825700).)

## Installation

You can install the released version of surveyharmonize from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("surveyharmonize")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("antaldaniel/retroharmonize")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(surveyharmonize)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
library(surveyharmonize)
library(labelled)

this <- read_spss ( 
  system.file("examples", "iris.sav", package = "haven"),
  id = 'iris1')

that <- read_spss ( 
  system.file("examples", "iris.sav", package = "haven"),
  id = 'iris2')

this$Species[2] <- 9999
that$Species[3] <- 9998

this$Species <- labelled_spss(
  this$Species, c( labelled::val_labels ( this$Species),
                   "missing" = 9999),
  na_values = 9999)

that$Species <- labelled_spss(
  that$Species, c( labelled::val_labels ( that$Species), 
                   "missing" = 9999),
  na_values = 9998)
```

## Code of Conduct

Please note that the surveyharmonize project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
