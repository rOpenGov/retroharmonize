#' retroharmonize: Retrospective harmonization of survey data files
#'
#' The goal of \code{retroharmonize} is to facilitate retrospective (ex-post) 
#' harmonization of data, particularly survey data, in a reproducible manner. 
#' The package provides tools for organizing the metadata, standardizing the 
#' coding of variables, variable names and value labels, including missing 
#' values, and for documenting all transformations, with the help of 
#' comprehensive S3 classes.
#'
#' @section import functions:
#' Read data stored in formats with rich metadata, such as SPSS (.sav) files, 
#' and make them usable in a programmatic context.\cr 
#' \code{\link{read_spss}}: read an SPSS file and record metadata for reproducibility\cr 
#' \code{\link{read_rds}}: read an rds file and record metadata for reproducibility\cr 
#' \code{\link{read_surveys}}: programmatically read a list of surveys\cr 
#' \code{\link{pull_survey}}: pull a single survey from a survey list.\cr 
#' 
#' @section subsetting functions:
#' \code{\link{subset_surveys}}: remove variables from surveys that cannot be harmonized.
#' 
#' @section variable name harmonization functions:
#' \code{\link{harmonize_survey_variables}}: Create a list of surveys with harmonized variable names.\cr
#' @section codebook functions:
#' \code{\link{codebook_create}}: A not yet working function.
#' @section codelist functions:
#' \code{\link{codelist_create}}: A not yet working function.
#' @section variable label harmonization functions:
#' Create consistent coding and labelling.\cr 
#' \code{\link{harmonize_values}}: Harmonize the label list across surveys.\cr
#' \code{\link{harmonize_survey_values}}: Create a list of surveys with harmonized value labels.\cr
#' \code{\link{na_range_to_values}}: Make the \code{na_range} attributes,
#' as imported from SPSS, consistent with the \code{na_values} attributes.\cr
#' \code{\link{label_normalize}} removes special characters, whitespace, 
#' and other typical typing errors and helps the uniformization of labels
#' and variable names.\cr
#' 
#' @section survey harmonization functions:
#' \code{\link{merge_surveys}}: Create a list of surveys with harmonized names and variable labels.\cr 
#' \code{\link{crosswalk_surveys}}: Create a list of surveys with harmonized variable names, harmonized 
#' value labels and harmonize R classes.\cr
#' \code{\link{crosswalk}}: Create a joined data frame of surveys with harmonized variable names, harmonized 
#' value labels and harmonize R classes.\cr
#' @section metadata functions:
#' \code{\link{metadata_create}}: Createa metadata dataa from one or more \code{\link{survey}}.\cr 
#' \code{\link{metadata_survey_create}}: Create a joined metadata data frame from one survey.\cr 
#' \code{\link{create_codebook}} and \code{\link{codebook_waves_create}}
#' \code{\link{crosswalk_table_create}}: Create an initial crosswalk table from a metadata data frame.\cr
#' 
#' @section documentation functions:
#' Make the workflow reproducible by recording the harmonization process.
#' \code{\link{document_survey_item}}: Returns a list of the current and historic coding, 
#' labelling of the valid range and missing values or range, the history of the variable names 
#' and the history of the survey IDs.
#' \code{\link{document_surveys}}: Document the key attributes surveys in a survey list.
#' 
#' @section type conversion functions:
#' Consistently treat labels and SPSS-style user-defined missing 
#' values in the R language.
#' \code{\link{survey}} helps constructing a valid survey data frame, and
#' \code{\link{labelled_spss_survey}} helps creating a vector for a 
#' questionnaire item.
#' \code{\link{as_numeric}}: convert to numeric values.\cr
#' \code{\link{as_factor}}: convert to labels to factor levels.\cr
#' \code{\link{as_character}}: convert to labels to characters.\cr
#' \code{\link{as_labelled_spss_survey}}: convert labelled and labelled_spss
#' vectors to labelled_spss_survey vectors.\cr
#' @docType package
#' @importFrom stringr str_extract
#' @import vctrs
#' @import rlang
#' @name retroharmonize
NULL