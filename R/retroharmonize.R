#' retroharmonize: Retrospective harmonization of survey data files
#'
#' The goal of \code{retroharmonize} is to allow the organization of data 
#' joins or panels from various data sources, particularly survey 
#' microdata files, by retrospective harmonization the value codes, 
#' the value labels, and the missing value ranges of the data in a 
#' reproducible manner with the help of comprehensive s3 classes.
#'
#' @section import functions:
#' The naming functions make the GESIS SPSS files usable in a 
#' programmatic context.
#' @section harmonization functions:
#' Creating consistent coding and labelling.
#' @section type conversion functions:
#' Consistently treat labels and SPSS-style user-defined missing 
#' values in the R language.
#' \code{\link{as_numeric}}: convert to numeric values
#' \code{\link{as_factor}}: convert to labels to factor levels
#' @docType package
#' @import vctrs
#' @name retrohamonize
NULL

