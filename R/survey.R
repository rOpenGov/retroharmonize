#' @title Create a survey data frame
#'
#' @description Store the data of a survey in a tibble (data frame) with a unique
#' survey identifier, import filename, and optional document object identifier.
#'
#' @details Whilst you can create a survey object with this helper function, it is most likely that
#' you will receive it with an importing function, i.e.
#' \code{\link{read_rds}}, \code{\link{read_spss}} \code{\link{read_dta}}, \code{\link{read_csv}} or
#' their common wrapper \code{\link{read_survey}}.
#'
#' @param object A tibble or data frame that contains the survey data.
#' @param id A mandatory identifier for the survey.
#' @param filename The import file name.
#' @param doi Optional document object identifier (doi), can be omitted.
#' @return A tibble with \code{id},  \code{filename},  \code{doi}
#' metadata information.
#' @family importing functions
#' @examples
#' example_survey <- survey(
#'   object = data.frame(
#'     rowid = 1:6,
#'     observations = runif(6)
#'   ),
#'   id = "example",
#'   filename = "no_file"
#' )
#' @export

survey <- function(object = data.frame(),
                   id = "survey_id",
                   filename = NULL,
                   doi = NULL) {
  new_survey(object, id, filename, doi)
}

## Validator for survey class, not to be exported ---------------------

#' @keywords internal
#' @importFrom assertthat assert_that
validate_survey <- function(object, id, filename, doi) {
  
  
  if (!is.null(doi)) {
    if (!is.character(doi) || length(doi) != 1) {
      stop("The 'doi' must be a character of length 1L (or NULL)")
    }
  }
  
  if (!is.null(filename)) {
    if (!is.character(filename) || length(filename) != 1) {
      stop("The 'filename' must be a character of length 1L (or NULL)")
    }
  }
  
  if (!is.null(id)) {
    if (!is.character(id) || length(id) != 1) {
      stop("The 'id' must be a character of length 1L (or NULL)")
    }
  }
  
  assertthat::assert_that(
    inherits(object, "data.frame"),
    msg = "df in validate_survey(df) must be a data.frame like object."
  )
}

## Constructor for new survey, not to be exported ---------------------
#' @keywords internal
#' @importFrom tibble tibble
new_survey <- function(object = tibble::tibble(),
                       id = character(1),
                       filename = character(1),
                       doi = character(1)) {
  validate_survey(object = object, 
                  id = id, 
                  filename = filename, 
                  doi = doi)
  
  structure(object,
            id = id,
            filename = filename,
            doi = doi,
            class = c("survey", class(object))
  )
}


#' @rdname survey
#' @export
is.survey <- function(object) {
  inherits(object, "survey") | inherits(object, "survey_df")
}

#' @rdname survey_df
is.survey_df <- function(object) {
  inherits(object, "survey") | inherits(object, "survey_df")
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

  print(summary(tibble::as_tibble(object)))

  not_yet_implement <- function() {
    labelled_types <- names(object)[vapply(object, function(x) inherits(x, "haven_labelled"), logical(1))]
    not_labelled_types <- object[!names(object) %in% labelled_types]

    labelled_cols <- object %>%
      select(all_of(labelled_types))

    numeric_cols <- labelled_cols %>%
      set_names(paste0(names(labelled_cols), "_numeric")) %>%
      mutate_all(as_numeric)

    factor_cols <- labelled_cols %>%
      set_names(paste0(names(labelled_cols), "_factor")) %>%
      mutate_all(as_factor)

    object %>%
      select(-all_of(labelled_types)) %>%
      bind_cols(numeric_cols) %>%
      bind_cols(factor_cols) %>%
      as_tibble() %>%
      summary() %>%
      print()
  }
}
