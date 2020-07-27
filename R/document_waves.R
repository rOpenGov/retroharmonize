#' @title Document survey lists
#'
#' @param survey_list A list of \code{\link{survey}} objects.
#' @return Returns a data frame with the key attributes of the surveys in
#' a survey list.
#' @importFrom tibble tibble
#' @importFrom here here
#' @family documentation functions
#' @examples
#' survey_list <- dir (
#'     here( "inst", "examples"))[grepl(".rds", 
#'                         dir (here( "inst", "examples")))]
#'                         
#' example_surveys <- read_surveys(
#'     here( "inst", "examples", survey_list))
#'     
#' waves_document <- document_waves(example_surveys)
#' 
#' attr(waves_document, "original_list" )
#' waves_document
#' @export

document_waves <- function( survey_list ) {
  
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
