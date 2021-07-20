
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retroharmonize

<!-- badges: start -->

[![Project Status: Active. The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/retroharmonize)](https://cran.r-project.org/package=retroharmonize)
[![CRAN\_time\_from\_release](https://www.r-pkg.org/badges/ago/retroharmonize)](https://cran.r-project.org/package=retroharmonize)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/retroharmonize)](https://cran.r-project.org/package=retroharmonize)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5117890.svg)](https://doi.org/10.5281/zenodo.5117890)
[![Codecov test
coverage](https://codecov.io/gh/antaldaniel/retroharmonize/branch/master/graph/badge.svg)](https://codecov.io/gh/antaldaniel/retroharmonize?branch=master)
[![R-CMD-check](https://github.com/antaldaniel/retroharmonize/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/retroharmonize/actions)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
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

## Installation

The package is available on [CRAN](https://CRAN.R-project.org):

``` r
install.packages("retroharmonize")
```

The development version has new features with the `create_codebook()`
functions. It can be installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rOpenGov/retroharmonize")
```

You can download the manual in PDF for the [0.1.17 CRAN
release](https://retroharmonize.dataobservatory.eu/retroharmonize_0.1.17.pdf)
and for the [0.1.19 development
release](https://retroharmonize.dataobservatory.eu/retroharmonize_0.1.19.pdf).

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
[here](https://retroharmonize.dataobservatory.eu/reference/retrohamonize.html)
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
Class](https://retroharmonize.dataobservatory.eu/articles/labelled_spss_survey.html)
provides more information about the `labelled_spss_survey()` class.

In [Harmonize Value
Labels](https://retroharmonize.dataobservatory.eu/articles/harmonize_labels.html)
we discuss the characteristics of the `labelled_spss_survey()` class and
demonstrates the problems that using this class solves.

We also provide three extensive case studies illustrating how the
`retroharmonize` package can be used for ex-post harmonization of data
from cross-national surveys:

-   [Afrobarometer](https://retroharmonize.dataobservatory.eu/articles/afrobarometer.html)
-   [Arab
    Barometer](https://retroharmonize.dataobservatory.eu/articles/arabbarometer.html)
-   [Eurobarometer](https://retroharmonize.dataobservatory.eu/articles/eurobarometer.html)

The creators of `retroharmonize` are not affiliated with either
Afrobarometer, Arab Barometer, Eurobarometer, or the organizations that
designs, produces or archives their surveys.

We started building an experimental APIs data is running retroharmonize
regularly and improving known statistical data sources. See: [Digital
Music Observatory](https://music.dataobservatory.eu/), [Green Deal Data
Observatory](https://greendeal.dataobservatory.eu/), [Economy Data
Observatory](https://economy.dataobservatory.eu/).

## Citations and related work

### Citing the data sources

Our package has been tested on three harmonized survey’s microdata.
Because [retroharmonize](https://retroharmonize.dataobservatory.eu/) is
not affiliated with any of these data sources, to replicate our
tutorials or work with the data, you have download the data files from
these sources, and you have to cite those sources in your work.

**Afrobarometer** data: Cite
[Afrobarometer](https://afrobarometer.org/data/) **Arab Barometer**
data: cite [Arab
Barometer](https://www.arabbarometer.org/survey-data/data-downloads/).
**Eurobarometer** data: The
[Eurobarometer](https://ec.europa.eu/commfrontoffice/publicopinion/index.cfm)
data
[Eurobarometer](https://ec.europa.eu/commfrontoffice/publicopinion/index.cfm)
raw data and related documentation (questionnaires, codebooks, etc.) are
made available by *GESIS*, *ICPSR* and through the *Social Science Data
Archive* networks. You should cite your source, in our examples, we rely
on the
[GESIS](https://www.gesis.org/en/eurobarometer-data-service/search-data-access/data-access)
data files.

### Citing the retroharmonize R package

For main developer and contributors, see the
[package](https://retroharmonize.dataobservatory.eu/) homepage.

This work can be freely used, modified and distributed under the GPL-3
license:

``` r
citation("retroharmonize")
#> 
#> To cite package 'retroharmonize' in publications use:
#> 
#>   Daniel Antal (2021). retroharmonize: Ex Post Survey Data
#>   Harmonization. R package version 0.1.18.
#>   https://retroharmonize.dataobservatory.eu/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {retroharmonize: Ex Post Survey Data Harmonization},
#>     author = {Daniel Antal},
#>     year = {2021},
#>     note = {R package version 0.1.18},
#>     url = {https://retroharmonize.dataobservatory.eu/},
#>   }
```

### Contact

For contact information, see the
[package](https://retroharmonize.dataobservatory.eu/) homepage.

### Code of Conduct

Please note that the `retroharmonize` project is released with a
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
