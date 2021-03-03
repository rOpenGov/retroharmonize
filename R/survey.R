## Constructor for new survey, not to be exported ---------------------
new_survey <- function ( df = tibble::tibble(), 
                         id = character(1), 
                         filename = character(1), 
                         doi = character(1)) { 
  
 validate_survey (df, id, filename, doi)

 structure(df, 
           id = id, 
           filename = filename, 
           doi = doi, 
           class = c("survey", class ( df)))
  
}

## Validator for survey class, not to be exported ---------------------

validate_survey <- function (df, id, filename, doi ) {
  
  if ( !is.null(filename)) {
    if ( (class (filename) != "character") && (length(filename) != 1) ) {
      stop ("The 'filename' must be a character of length 1L (or NULL)")
    } 
  }
  
  if ( !is.null(doi)) {
    if ( (class (doi) != "character") && (length(doi) != 1) ) {
      stop ("The 'doi' must be a character of length 1L (or NULL)")
    } 
    
  }
  
  if ( (class (id) != "character") && (length(id) != 1) ) stop ("The id must be a character of length 1L")
  
  assert_that(
    inherits(df, "data.frame"),
    msg =  "df in validate_survey(df) must be a data.frame like object."
  )
}

## Helper function to construct a survey data frame ------------------

#' Survey data frame
#' 
#' Store the data of a survey in a tibble (data frame) with a unique
#' survey identifier, import filename, and optional doi.
#' 
#' @param df A tibble or data frame that contains the survey data.
#' @param id A mandatory identifier for the survey
#' @param filename The import file name.
#' @param doi Optional doi, can be omitted.
#' @return A tibble with \code{id},  \code{filename},  \code{doi} 
#' metadata information.
#' @examples
#' example_survey <- survey( 
#'   df =data.frame ( 
#'     rowid = 1:6,
#'     observations = runif(6)), 
#'   id = 'example', 
#'   filename = "no_file"
#' )
#' @export

survey <- function ( df = data.frame(),
                     id = character(), 
                     filename = character(), 
                     doi = character()) { 
  new_survey (df, id, filename, doi)
  }

#' @rdname survey
#' @export
is.survey <- function (df) {
  inherits(df, "survey")
}
