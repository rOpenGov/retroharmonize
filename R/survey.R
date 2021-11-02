## Constructor for new survey, not to be exported ---------------------
new_survey <- function ( object = tibble::tibble(), 
                         id = character(1), 
                         filename = character(1), 
                         doi = character(1)) { 
  
 validate_survey (object, id, filename, doi)

 structure(object, 
           id = id, 
           filename = filename, 
           doi = doi, 
           class = c("survey", class ( object)))
  
}

## Validator for survey class, not to be exported ---------------------

validate_survey <- function (object, id, filename, doi ) {
  
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
    inherits(object, "data.frame"),
    msg =  "df in validate_survey(df) must be a data.frame like object."
  )
}

## Helper function to construct a survey data frame ------------------

#' Survey data frame
#' 
#' Store the data of a survey in a tibble (data frame) with a unique
#' survey identifier, import filename, and optional doi.
#' 
#' @param object A tibble or data frame that contains the survey data.
#' @param id A mandatory identifier for the survey
#' @param filename The import file name.
#' @param doi Optional doi, can be omitted.
#' @return A tibble with \code{id},  \code{filename},  \code{doi} 
#' metadata information.
#' @examples
#' example_survey <- survey( 
#'   object =data.frame ( 
#'     rowid = 1:6,
#'     observations = runif(6)), 
#'   id = 'example', 
#'   filename = "no_file"
#' )
#' @export

survey <- function ( object = data.frame(),
                     id = character(), 
                     filename = character(), 
                     doi = character()
                     ) { 
  new_survey (object, id, filename, doi)
  }

#' @rdname survey
#' @export
is.survey <- function (object) {
  inherits(object, "survey")
}

#' @rdname survey
#' @param ... Arguments passed to summary method.
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols mutate_all all_of
#' @importFrom purrr set_names
#' @export
summary.survey <- function(object, ...) {
  if (!is.null(attr(object, "label"))) {
    cat(attr(object, "label"))
  }
  
  print(summary(as_tibble(object)))
  
  not_yet_implement <- function() {
    labelled_types <- names(object)[vapply ( object, function(x)inherits(x, "haven_labelled"), logical(1))]
    not_labelled_types <- object[!names(object) %in% labelled_types]
    
    labelled_cols <- object %>% 
      select ( all_of(labelled_types))  
    
    numeric_cols <- labelled_cols %>%
      set_names(  paste0(names(labelled_cols), "_numeric")) %>%
      mutate_all ( as_numeric )
    
    factor_cols <- labelled_cols %>%
      set_names(  paste0(names(labelled_cols), "_factor")) %>%
      mutate_all ( as_factor )
    
    object %>% select ( -all_of(labelled_types)) %>%
      bind_cols(numeric_cols) %>%
      bind_cols(factor_cols) %>%
      as_tibble() %>%
      summary() %>%
      print()
  }
}