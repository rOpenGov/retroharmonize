#' @title Create a survey data frame
#' 
#' @description Store the data of a survey in a tibble (data frame) with a unique
#' survey identifier, import filename, and optional document object identifier.
#' @details Whilst you can create a survey object with this helper function, 
#' it is most likely that you will receive it with an importing function, i.e. 
#' \code{\link{read_rds}}, \code{\link{read_spss}} \code{\link{read_dta}}, 
#' \code{\link{read_csv}} or 
#' their common wrapper \code{\link{read_survey}}.
#' @param x A tibble or data frame that contains the survey data.
#' @param dataset_bibentry A list of bibliographic references and descriptive metadata
#' about the dataset as a whole created with 
#' \code{\link[dataset:datacite]{dataset::datacite}} or
#' \code{\link[dataset:dublincore]{dataset::dublincore}}.
#' @param dataset_subject The subject of the dataset; it can be a simple 
#' character string, or a more complex object created with 
#' \code{\link[dataset:subject]{dataset::subject}}.
#' @param id A mandatory identifier for the survey.
#' @param filename The import file name.
#' @return A \code{\link{dataset::dataset_df}} object with with \code{id}, and   
#' \code{filename} metadata information.
#' @importFrom dataset dataset_df datacite dublincore subject subject_create
#' @importFrom dataset `subject<-`
#' @family importing functions
#' @examples
#' example_survey <- survey( 
#'   x = data.frame ( 
#'     rowid = 1:6,
#'     observations = runif(6)), 
#'   id = 'example', 
#'   filename = "no_file"
#' )
#' @export

survey_df <- function (x, 
                       dataset_bibentry = NULL, 
                       dataset_subject = NULL,
                       identifier, 
                       filename ) {
  
  if (is.null(dataset_bibentry)) {
    Title   <- "Untitled Dataset"
    Creator <- person("Author", "Unknown")
    dataset_bibentry <- dataset::datacite(Title=Title, Creator=Creator, Subject=dataset_subject)
  }
  
  if (is.null(dataset_subject)){
    dataset_subject <- subject_create(term="data sets",
                                      subjectScheme="Library of Congress Subject Headings (LCSH)",
                                      schemeURI="https://id.loc.gov/authorities/subjects.html",
                                      valueURI="http://id.loc.gov/authorities/subjects/sh2018002256",
                                      classificationCode = NULL,
                                      prefix = "")
    }
  
  if(!is.dataset_df(x)) {
    tmp <- dataset_df(x, dataset_bibentry = dataset_bibentry, identifier=identifier, filename=filename )
  } else {
    tmp <- x
    identifier(tmp) <- identifier
  }
  
  attr(tmp, "filename") <- filename
  attr(tmp, "id") <- identifier
  class(tmp) <- c("survey_df", class(tmp))
  tmp
}

#' @rdname survey_df
#' @export
is.survey_df <- function (object) {
  inherits(object, "survey") | inherits(object, "survey_df")
}


#' @rdname survey_df
#' @export
print.survey_df <- function(x, ...) {
  NextMethod()
}
