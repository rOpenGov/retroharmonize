#' @title Merge surveys
#'
#' @description
#' Merge a list of surveys into a list with harmonized variable names,
#' variable labels, and survey identifiers.
#'
#' @details
#' Prior to version 0.2.0 this function was called \code{merge_waves()},
#' reflecting the terminology used in Eurobarometer surveys.
#'
#' @param survey_list
#' A list of surveys.
#'
#' @param var_harmonization
#' A metadata table describing how variables should be harmonized.
#' It must contain at least the columns
#' \code{filename}, \code{var_name_orig}, \code{var_name_target},
#' and \code{var_label}.
#'
#' @return
#' A list of surveys with harmonized variable names and labels.
#'
#' @family survey harmonization functions
#'
#' @seealso metadata_create
#'
#' @importFrom rlang set_names .data
#' @importFrom dplyr select mutate filter across any_of
#' @importFrom haven is.labelled
#' @importFrom tidyselect all_of
#'
#' @examples
#' \donttest{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_files <- dir(examples_dir, pattern = "\\.rds$", full.names = TRUE)
#'
#' example_surveys <- read_surveys(
#'   survey_files,
#'   save_to_rds = FALSE
#' )
#'
#' # Create metadata from surveys
#' metadata <- metadata_create(survey_list = example_surveys)
#'
#' # Select and harmonize a subset of variables
#' to_harmonize <- metadata %>%
#'   dplyr::filter(
#'     var_name_orig %in% c("rowid", "w1") |
#'       grepl("^trust", var_label_orig)
#'   ) %>%
#'   dplyr::mutate(
#'     var_label = var_label_normalize(var_label_orig),
#'     var_name_target = val_label_normalize(var_label),
#'     var_name_target = ifelse(
#'       .data$var_name_orig %in% c("rowid", "w1", "wex"),
#'       .data$var_name_orig,
#'       .data$var_name_target
#'     )
#'   )
#'
#' merged_surveys <- merge_surveys(
#'   survey_list = example_surveys,
#'   var_harmonization = to_harmonize
#' )
#'
#' merged_surveys[[1]]
#' }
#'
#' @export


merge_surveys <- function(survey_list, var_harmonization) {
  validate_survey_list(survey_list)

  if (any(!c("filename", "var_name_orig", "var_name_target", "var_label") %in% names(var_harmonization))) {
    stop(
      "var_harmonization must contain ",
      paste(c("filename", "var_name_orig", "var_name_target", "var_label"), collapse = ", "),
      "."
    )
  }

  fn_merge <- function(dat) {
    select_vars <- var_harmonization %>%
      filter(.data$filename == attr(dat, "filename"))

    if (!"rowid" %in% select_vars$var_name_orig) {
      warning("rowid is not selected from ", attr(dat, "filename"))
    }

    tmp <- dat %>%
      select(all_of(c(select_vars$var_name_orig))) %>%
      rlang::set_names(nm = select_vars$var_name_target)

    labelled_vars <- names(tmp)[vapply(tmp, haven::is.labelled, logical(1))]

    if (length(labelled_vars) > 0) {
      fn_relabel <- function(x) as_labelled_spss_survey(x, id = attr(tmp, "id"))
      tmp <- tmp %>%
        mutate(across(any_of(labelled_vars), fn_relabel))
    }

    if (!is.null(select_vars$var_label)) {
      labelled_items <- vapply(tmp, is.labelled_spss_survey, logical(1))
      labelled_items <- names(labelled_items)[labelled_items]

      labelling <- select_vars %>%
        select(all_of(c("var_name_target", "var_label")))

      fn_relabelling <- function(x) {
        labelling$var_label[which(labelling$var_name_target == x)]
      }

      for (x in labelled_items) {
        attr(tmp[[x]], "label") <- fn_relabelling(x)
      }
    }

    tmp
  }

  lapply(survey_list, fn_merge)
}

#' @title merge_waves
#' @rdname merge_surveys
#' @param waves Deprecated.
#' @export
merge_waves <- function(waves, var_harmonization) {
  merge_surveys(
    survey_list = waves,
    var_harmonization = var_harmonization
  )
}
