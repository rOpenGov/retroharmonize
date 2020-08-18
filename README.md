
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retroharmonize

<!-- badges: start -->

[![Project Status: Minimal or no implementation has been done yet, or
the repository is only intended to be a limited example, demo, or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3937746.svg)](https://doi.org/10.5281/zenodo.3937746)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
[![Codecov test
coverage](https://codecov.io/gh/antaldaniel/retroharmonize/branch/master/graph/badge.svg)](https://codecov.io/gh/antaldaniel/retroharmonize?branch=master)
[![Travis build
status](https://travis-ci.com/antaldaniel/retroharmonize.svg?branch=master)](https://travis-ci.com/antaldaniel/retroharmonize)
<!-- badges: end -->

The goal of `retroharmonize` is to facilitate retrospective (ex-post)
harmonization of data, particularly survey data, in a reproducible
manner. The package provides tools for organizing the metadata,
standardizing the coding of variables, variable names and value labels,
including missing values, and for documenting all transformations, with
the help of comprehensive S3 classes.

Currently being generalized from problems solved in the not yet released
[eurobarometer](https://github.com/antaldaniel/eurobarometer) package
([doi](https://doi.org/10.5281/zenodo.3825700).)

You can download the manual in [PDF](retroharmonize_0.1.9.pdf).

## Installation

The package will be available for install via after review on
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("retroharmonize")
```

The development version from [GitHub](https://github.com/) can be
installed with:

``` r
# install.packages("devtools")
devtools::install_github("antaldaniel/retroharmonize")
```

## Retrospective data harmonization

The aim of `retroharmonize` is to provide tools for reproducible
retrospective (ex-post) harmonization of datasets that contain variables
measuring the same concepts but coded in different ways. Ex-post data
harmonization enables better use of existing data and creates new
research opportunities. For example, harmonizing data from different
countries enables cross-national comparisons, while merging data from
different time points makes it possible to track changes over time.

Retrospective data harmonization is associated with challenges including
conceptual issues with establishing equivalence and comparability,
practical complications of having to standardize the naming and coding
of variables, technical difficulties with merging data stored in
different formats, and the need to document a large number of data
transformations. The `retroharmonize` package assists with the latter
three components, freeing up the capacity of researchers to focus on the
first.

Specifically, the `retroharmonize` package proposes a reproducible
workflow, including a new class for storing data together with the
harmonized and original metadata, as well as functions for importing
data from different formats, harmonizing data and metadata, documenting
the harmonization process, and converting between data types. See
[here](http://retroharmonize.satellitereport.com/reference/retrohamonize.html)
for an overview of the functionalities.

The new `labelled_spss_survey()` class is an extension of [haven’s
labelled\_spss
class](https://haven.tidyverse.org/reference/labelled_spss.html). It not
only preserves variable and value labels and the user-defined missing
range, but also gives an identifier, for example, the filename or the
wave number, to the vector. Additionally, it enables the preservation –
as metadata attributes – of the original variable names, labels, and
value codes and labels, from the source data, in addition to the
harmonized variable names, labels, and value codes and labels. This way,
the harmonized data also contain the pre-harmonization record. The
stored original metadata can be used for validation and documentation
purposes.

The vignette [Working With The labelled\_spss\_survey
Class](http://retroharmonize.satellitereport.com/articles/labelled_spss_survey.html)
provides more information about the `labelled_spss_survey()` class.

In [Harmonize Value
Labels](http://retroharmonize.satellitereport.com/articles/harmonize_labels.html)
we discuss the characteristics of the `labelled_spss_survey()` class and
demonstrates the problems that using this class solves.

We also provide two extensive case studies illustrating how the
`retroharmonize` package can be used for ex-post harmonization of data
from cross-national surveys on the example of the
[Afrobarometer](http://retroharmonize.satellitereport.com/articles/afrobarometer.html)
and the
[Eurobarometer](http://retroharmonize.satellitereport.com/articles/eurobarometer.html).
The creators of `retroharmonize` are not affiliated with either
Afrobarometer, Eurobarometer, or the organizations that designs,
produces or archives their surveys.

## Code of Conduct

Please note that the `retroharmonize` project is released with a
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
