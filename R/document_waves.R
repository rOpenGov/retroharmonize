#' @title Document survey lists
#' 
#' @description Document the key attributes surveys in a survey list.
#'
#' @param survey_list A list of \code{\link{survey}} objects.
#' @return Returns a data frame with the key attributes of the surveys 
#' in a survey list: the name of the data file, the number of rows and 
#' columns, and the size of the object as stored in memory.
#' @importFrom tibble tibble
#' @importFrom here here
#' @family documentation functions
#' @examples
#' examples_dir <- system.file( "examples", package = "retroharmonize")
#'                         
#' my_rds_files <- dir( examples_dir)[grepl(".rds", 
#'                                    dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))
#'  
#' documented_surveys <- document_surveys(example_surveys)
#' 
#' attr(documented_surveys, "original_list" )
#' documented_surveys
#' @export

document_surveys <- function( survey_list ) {
  
  validate_survey_list(survey_list)
  
  n_survey <- length(survey_list)
  
  tmp <- tibble(
    id  = vapply ( survey_list, function(x) attr(x, "id"), character(1)), 
    filename =  vapply ( survey_list, function(x) attr(x, "filename"), character(1)), 
    ncol = vapply (survey_list, ncol, integer(1)), 
    nrow =  vapply ( survey_list, nrow, integer(1)),
    object_size =  vapply ( survey_list, object.size, double(1))
    )
  
  attr(tmp, "original_list")  <- deparse(substitute(survey_list))
  
  tmp
}


#' @rdname document_surveys
#' @details The earlier form \code{document_waves} is deprecated.  
#' Currently called \code{\link{document_surveys}}.
#' @inheritParams document_surveys

document_waves <- function(survey_list, var_harmonization) {
  message ("This function will be deprecated. Use document_sureys() instead")
  merge_surveys(suvey_list = survey_list, 
                var_harmonization = var_harmonization)
}
