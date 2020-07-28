#' retroharmonize: Retrospective harmonization of survey data files
#'
#' The goal of `retroharmonize` is to facilitate retrospective (ex-post) 
#' harmonization of data, particularly survey data, in a reproducible manner. 
#' The package provides tools for organizing the metadata, standardizing the 
#' coding of variables, variable names and value labels, including missing 
#' values, and for documenting all transformations, with the help of 
#' comprehensive s3 classes.
#'
#' @section import functions:
#' Make data stored in formats with rich metadata, such as SPSS (.sav) files, 
#' usable in a programmatic context.
#' @section harmonization functions:
#' Create consistent coding and labelling.
#' @section documentation functions:
#' Make the workflow reproducible by recording the harmonization process.
#' @section type conversion functions:
#' Consistently treat labels and SPSS-style user-defined missing 
#' values in the R language.
#' \code{\link{as_numeric}}: convert to numeric values
#' \code{\link{as_factor}}: convert to labels to factor levels
#' \code{\link{as_character}}: convert to labels to characters
#' \code{\link{as_labelled_spss_survey}}: convert labelled and labelled_spss
#' vectors to labelled_spss_survey vectors.
#' @docType package
#' @import vctrs
#' @import rlang
#' @name retrohamonize
NULL

