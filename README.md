
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retroharmonize <a href='https://retroharmonize.dataobservatory.eu/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](http://ropengov.org/)
[![Project Status: Active. The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/retroharmonize)](https://cran.r-project.org/package=retroharmonize)
[![CRAN\_time\_from\_release](https://www.r-pkg.org/badges/ago/retroharmonize)](https://cran.r-project.org/package=retroharmonize)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/retroharmonize)](https://cran.r-project.org/package=retroharmonize)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5764361.svg)](https://doi.org/10.5281/zenodo.5764361)
[![codecov](https://codecov.io/gh/ropengov/retroharmonize/branch/master/graph/badge.svg?token=ShTTeqB5kL)](https://app.codecov.io/gh/ropengov/retroharmonize)
[![R-CMD-check](https://github.com/rOpenGov/retroharmonize/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/retroharmonize/actions)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
<!-- badges: end -->

The goal of `retroharmonize` is to facilitate retrospective (ex-post)
harmonization of survey data in a reproducible manner. The package
provides tools for organizing the metadata, standardizing the coding of
variables, variable names and value labels, including missing values,
and for documenting all transformations, with the help of comprehensive
S3 classes.

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

You can download the manual in PDF for the [0.2.3
release](https://retroharmonize.dataobservatory.eu/retroharmonize_0.2.3.pdf).

## Survey harmonization

Survey data harmonization refers to procedures that improve the data
comparability or the inferential capacity of multiple surveys. The
*retroharmonize* package support various data processing, documentation,
file/type conversion aspects of various survey harmonization workflows.

From a technical perspective, the aim of the survey harmonization is to
create a single, tidy, joined table in the form of a data frame that
contains a row identifier, which is truly unique across all observations
and the concatenated and harmonized variables.

1.  **Harmonization of concepts**: Create a mental map of the measured
    concepts that needs to be harmonized, for example, a binary sex
    variable with missing cases and a four-level categorical variable on
    gender identification that has *other* and *declined* options. See
    the vignette [Working With Survey
    Metadata](https://retroharmonize.dataobservatory.eu/articles/survey_harmonization)
    how mapping the metadata of the surveys can help getting started
    with this first step.
2.  **Variable names**: Make sure that `survey_1$sex` and
    `survey_2$gender` can be concatenated to a gender vector or
    `survey_joined$gender`. See more in the [Working With A Crosswalk
    Table](https://retroharmonize.dataobservatory.eu/articles/crosswalk.html).
3.  **Variable coding and labels**: Female=0 in `survey_1$sex` and
    female=2 in `survey_2$gender` becomes consistently female=0. Missing
    and *declined* values are consistently handled.
4.  **Variable types**: to use R’s statistical functions with the
    concatenated version of `survey_1$sex` and `survey_2$gender` they
    must have the same R type. In the vast majority of the cases either
    *numeric* or *factor*, and in data visualization applications
    sometimes *character*. See more in the [Harmonize Value
    Labels](https://retroharmonize.dataobservatory.eu/articles/harmonize_labels.html)
    vignette.
5.  **Reproducibility**: To review statistics derived from the
    concatenated variable (or the joined data frame), they must remain
    comparable with `survey_1$sex` and `survey_2$gender`. It is also
    necessary to have a new, unique row ID for each observation. If you
    want to make your work available outside R, in a different software,
    the joined, longitudional data frame must be exported in a
    consistent manner.

For an extended overview of these problems with illustrations please
refer to the vignette [Survey
Harmonization](https://retroharmonize.dataobservatory.eu/articles/survey_harmonization).

Researchers may have different preferred workflows, for example:

1.  Harmonize concepts -&gt; Tidy datasets -&gt; Harmonize variable
    names -&gt; Harmonize variable classes -&gt; Harmonize labels and
    codes -&gt; Join data

2.  Harmonize concepts -&gt; Tidy datasets -&gt; Harmonize variable
    names -&gt; Harmonize labels and codes - &gt; Harmonize variable
    classes -&gt; Join data

3.  Harmonize concepts -&gt; Tidy datasets -&gt; Harmonize labels and
    codes -&gt; Harmonize variable classes -&gt; Harmonize names -&gt;
    Join data

Our package has different utilities for different workflows.

## Working with SPSS files

Survey data is often available in SPSS’s custom labelled format.
Unfortunately, joining data with different labelling is not possible.
When you do not need to preserve the history of complex harmonization
problems, codebook, etc, then you do not necessary need to look under
the hoods of our S3 classes. The new `labelled_spss_survey()` class is
an inherited extension of [haven’s labelled\_spss
class](https://haven.tidyverse.org/reference/labelled_spss.html). It not
only preserves variable and value labels and the user-defined missing
range, but also gives an identifier, for example, the filename or the
wave number, to the vector. Additionally, it enables the preservation—
as metadata attributes—the original variable names, labels, and value
codes and labels, from the source data. This way, the harmonized data
also contain the pre-harmonization record. The vignette [Working With
The labelled\_spss\_survey
Class](https://retroharmonize.dataobservatory.eu/articles/labelled_spss_survey.html)
provides more information about the `labelled_spss_survey()` class.

In [Harmonize Value
Labels](https://retroharmonize.dataobservatory.eu/articles/harmonize_labels.html)
we discuss the characteristics of the `labelled_spss_survey()` class and
demonstrates the problems that using this class solves.

## Use Cases

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
#>   Harmonization. https://retroharmonize.dataobservatory.eu/,
#>   https://github.com/rOpenGov/retroharmonize.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {retroharmonize: Ex Post Survey Data Harmonization},
#>     author = {Daniel Antal},
#>     year = {2021},
#>     note = {https://retroharmonize.dataobservatory.eu/,
#> https://github.com/rOpenGov/retroharmonize},
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
