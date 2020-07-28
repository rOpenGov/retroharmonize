#' retroharmonize: Retrospective harmonization of survey data files
#'
#' The goal of \code{retroharmonize} is to facilitate retrospective (ex-post) 
#' harmonization of data, particularly survey data, in a reproducible manner. 
#' The package provides tools for organizing the metadata, standardizing the 
#' coding of variables, variable names and value labels, including missing 
#' values, and for documenting all transformations, with the help of 
#' comprehensive s3 classes.
#'
#' @section import functions:
#' Read data stored in formats with rich metadata, such as SPSS (.sav) files, 
#' and make them usable in a programmatic context.
#' \code{\link{read_spss}}: read an SPSS file and record metadata for reproducibility
#' \code{\link{read_rds}}: read an rds file and record metadata for reproducibility
#' \code{\link{read_surveys}}: programmatically read a list of surveys
#' \code{\link{subset_save_surveys}}: programmatically read a list of surveys, 
#' and subset them (pre-harmonize the same variables.)
#' \code{\link{pull_survey}}: pull a single survey from a survey list.
#' @section harmonization functions:
#' Create consistent coding and labelling.
#' \code{\link{harmonize_values}}:
#' \code{\link{merge_waves}}: Create a list of surveys with harmonized names and variable labels.
#' \code{\link{harmonize_waves}}: Create a list of surveys with harmonized value labels.
#' \code{\link{label_normalize}}:
#' \code{\link{na_range_to_values}}: Make the \code{na_range} attributes,
#' as imported from SPSS, consistent with the \code{na_values} attributes.
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