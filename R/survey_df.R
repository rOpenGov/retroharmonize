#' Create a survey object
#'
#' Construct a survey object from a data frame or tibble by attaching
#' survey-level metadata such as an identifier, source filename, and
#' basic dataset-level descriptive metadata.
#'
#' This function is primarily intended for use by import helpers such as
#' [read_rds()], [read_spss()], [read_dta()], and [read_csv()].
#' Most users will not need to call it directly.
#'
#' @param x A data frame or tibble containing the survey data.
#' @param title Optional title for the survey. Defaults to `"Untitled Survey"`.
#' @param creator A [utils::person()] object describing the dataset creator.
#'   Defaults to `person("Unknown", "Creator")`.
#' @param dataset_bibentry Optional dataset-level bibliographic metadata.
#'   If `NULL`, a minimal DataCite entry is created automatically using
#'   `title`, `creator`, and `dataset_subject`.
#' @param dataset_subject Dataset subject metadata.
#'   If `NULL`, defaults to the Library of Congress Subject Heading
#'   \href{https://id.loc.gov/authorities/subjects/sh85130875}{Surveys}.
#' @param identifier A character scalar identifying the survey.
#' @param filename A character scalar giving the source filename, or `NULL`
#'   if unknown.
#'   
#' @param ... potentially further arguments for methods.
#'
#' @return
#' An object of class `"survey_df"`, which is a data frame with additional
#' survey-level metadata stored as attributes and dataset-level metadata
#' stored using the `dataset` package.
#'
#' @seealso
#' [read_survey()] for importing survey data from external files.
#'
#' @family importing functions
#' 
#' @importFrom utils bibentry person
#' @importFrom dataset is.dataset_df dataset_df datacite subject_create
#' @importFrom dataset 'identifier<-'
#'
#' @examples
#' survey_df(
#'   x = data.frame(
#'     rowid = 1:6,
#'     observations = runif(6)
#'   ),
#'   identifier = "example",
#'   filename = "no_file"
#' )
#'
#' @export

survey_df <- function(x,
                      title = NULL, 
                      creator = person("Unknown", "Creator"),
                      dataset_bibentry = NULL,
                      dataset_subject = NULL,
                      identifier,
                      filename) {
  
  if (is.null(dataset_subject)) {
    dataset_subject <- dataset::subject_create(
      term = "Surveys",
      subjectScheme = "Library of Congress Subject Headings (LCSH)",
      schemeURI = "https://id.loc.gov/authorities/subjects.html",
      valueURI = "http://id.loc.gov/authorities/subjects/sh85130875",
      classificationCode = NULL,
      prefix = ""
    )
  }
  
  Title <- if (is.null(title)) "Untitled Survey" else title
  
  Creator <- creator
  
  if (is.null(dataset_bibentry)) {
    dataset_bibentry <- dataset::datacite(Title = Title, 
                                          Creator = Creator, 
                                          Subject = dataset_subject)
  }

  if (!is.dataset_df(x)) {
    tmp <- dataset::dataset_df(x, 
                               dataset_bibentry = dataset_bibentry, 
                               identifier = identifier, 
                               filename = filename)
  } else {
    tmp <- x
    identifier(tmp) <- identifier
  }

  dataset::subject(tmp) <- dataset_subject
  attr(tmp, "filename") <- filename
  attr(tmp, "id") <- identifier
  class(tmp) <- c("survey_df", class(tmp))
  tmp
}

#' @rdname survey_df
#' @export
is.survey_df <- function(x) {
  inherits(x, "survey") | inherits(x, "survey_df")
}


#' @rdname survey_df
#' @export
print.survey_df <- function(x, ...) {
  NextMethod()
}
