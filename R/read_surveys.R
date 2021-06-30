#' @title Read Survey Files
#'
#' @description Import surveys into a list. Adds filename as a constant to each
#' element of the list.
#' 
#' @details The functions handle exceptions with wrong filenames and not readable 
#' files. If I file cannot be read, a warning is given, and empty survey is added to the 
#' the list in the place of this file. 
#'
#' @param import_file_names A vector of file names to import.
#' @param .f A function to import the surveys with.
#' Defaults to \code{'read_rds'}. For SPSS files,
#' \code{read_spss} is recommended, which is a
#' well-parameterized version of \code{\link[haven]{read_spss}} that
#' saves some metadata, too.
#' @param save_to_rds Should it save the imported survey to .rds?
#' Defaults to \code{FALSE}.
#' @return A list of the surveys.  Each element of the list is a data
#' frame-like \code{\link{survey}} type object where some metadata, 
#' such as the original file name, doi identifier if present, and other
#' information is recorded for a reproducible workflow.
#' @importFrom purrr safely
#' @importFrom fs path_file
#' @examples
#' file1 <- system.file(
#'     "examples", "ZA7576.rds", package = "retroharmonize")
#' file2 <- system.file(
#'     "examples", "ZA5913.rds", package = "retroharmonize")
#'
#' read_surveys (c(file1,file2), .f = 'read_rds' )
#' @export
#' @family import functions
#' @seealso survey

read_surveys <- function ( import_file_names,
                           .f = 'read_rds',
                           save_to_rds = FALSE ) {
 
  safely_read_survey <- function( filename, .f ) {
    
    if ( .f == 'read_spss' ) {
      message ( "Reading: ", fs::path_file(filename))
      
      tried_survey <- read_spss(file = filename, user_na = TRUE)
      
    } else if ( .f == 'read_rds') {
      
      tried_survey <- read_rds(file = filename)
      
    } else {
      stop ( "Other file types need to be fixed.")
      tried_survey <- purrr::safely (.f = .f )(file = filename)
      
      if ( is.null(tried_survey$error)) {
        tried_survey  <- tried_survey$result
      } else {
        warning("Survey ",fs::path_file(filename) , " could not be read and an emtpy survey will be inserted.
                \nThe following error message was received by haven::read_spss():\n", tried_survey$error)
        return ( survey( data.frame(), filename = filename, id = "not readable file"))
      }
    }
    
    if ( nrow(tried_survey) >0 & save_to_rds == TRUE ) {
      rds_filename <- gsub(".sav|.por", ".rds", filename)
      "Saving the survey to rds in the same location."
      saveRDS(tried_survey$result, rds_filename, version=2)
    }
 
     tried_survey
  }

  import_file_list <- as.list (import_file_names)
  
  tmp <- lapply ( import_file_list , FUN = function(x) safely_read_survey(x, .f)   )

  tmp
}

