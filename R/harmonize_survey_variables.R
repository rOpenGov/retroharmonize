#' Read a survey from a CSV file
#'
#' Import a survey stored in a CSV file and return it as a survey object
#' with attached dataset- and survey-level metadata.
#'
#' The CSV file is read using [utils::read.csv()]. Character variables
#' with more than one unique value are automatically converted to
#' labelled factors. A unique row identifier is added and labelled.
#' @param survey_list A list containing surveys of class survey.
#' @param subset_name Character string appended to filenames of
#'   subsetted surveys. Defaults to `"subset"`.
#' @param crosswalk_table A crosswalk table created with
#'   [crosswalk_table_create()].
#' @param survey_paths Optional character vector of file paths to surveys.
#'
#' @param import_path Optional base directory used to resolve `survey_paths`.
#'
#' @param export_path Optional directory where subsetted surveys are
#' exported to
#'
#' @details
#' If the file cannot be read, an empty survey object is returned with
#' a warning.
#'
#' If a column named `"X"` is present (commonly created by
#' `write.csv()`), it is removed automatically.
#'
#' @return
#' An object of class `"survey"`, which is a data frame with attached
#' survey- and dataset-level metadata.
#'
#' @seealso
#' [read_rds()] for importing surveys from RDS files,
#' [survey_df()] for constructing survey objects manually.
#'
#' @family import functions
#'
#' @importFrom dplyr left_join filter select distinct_all
#' @importFrom tibble tibble
#' @importFrom rlang set_names
#'
#' @examples
#' # Create a temporary CSV file from an example survey
#' path <- system.file("examples", "ZA7576.rds", 
#'                     package = "retroharmonize")
#' survey <- read_rds(path)
#'
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(survey, tmp, row.names = FALSE)
#'
#' # Read the CSV file back as a survey
#' re_read <- read_csv(
#'   file = tmp,
#'   id = "ZA7576",
#'   doi = "10.0000/example"
#' )
#'
#' @export


harmonize_survey_variables <- function(crosswalk_table,
                                       subset_name = "subset",
                                       survey_list = NULL,
                                       survey_paths = NULL,
                                       import_path = NULL,
                                       export_path = NULL) {
  # This is a wrapper for subset_save_survey with strict validation 
  # of new variable names.

  # validates structure and unambiguous naming
  is.crosswalk_table(crosswalk_table) 

  subsetted_surveys <- subset_surveys(
    crosswalk_table = crosswalk_table,
    subset_name = subset_name,
    survey_list = survey_list,
    import_path = import_path,
    export_path = export_path
  )

  rename_survey_to_memory <- function(x) {
    if (is.survey(x)) {
      this_survey <- x
    } else if (is.character(x)) {
      this_survey <- read_rds(file.path(export_path, x))
    } else {
      stop(" rename_survey_file(x) x must be a filename or a survey.")
    }

    survey_id <- gsub(paste0("_", subset_name), "", attr(this_survey, "id"))

    new_names <- tibble(var_name_orig = names(this_survey)) %>%
      left_join(
        crosswalk_table %>%
          filter(id == survey_id) %>%
          select(var_name_orig, var_name_target) %>%
          distinct_all(),
        by = "var_name_orig",
      ) %>%
      mutate(var_name_target = ifelse(var_name_orig == "rowid",
        yes = "rowid",
        no = var_name_target
      )) %>%
      select(var_name_target) %>%
      unlist() %>%
      as.character()

    rlang::set_names(this_survey, nm = new_names)
  }

  rename_survey_to_file <- function(x) {
    if (is.survey(x)) {
      this_survey <- x
    } else if (is.character(x)) {
      # this_survey <- read_rds(file.path(export_path, x))
      this_survey <- readRDS(file.path(export_path, x))
    } else {
      stop(" rename_survey_file(x) x must be a filename or a survey.")
    }

    survey_id <- gsub(paste0("_", subset_name), "", attr(this_survey, "id"))


    new_names <- tibble(var_name_orig = names(this_survey)) %>%
      left_join(
        crosswalk_table %>%
          filter(id == survey_id) %>%
          select(var_name_orig, var_name_target) %>%
          distinct_all(),
        by = "var_name_orig",
      ) %>%
      mutate(var_name_target = ifelse(var_name_orig == "rowid",
        yes = "rowid",
        no = var_name_target
      )) %>%
      select(var_name_target) %>%
      unlist() %>%
      as.character()

    this_survey <- rlang::set_names(this_survey, nm = new_names)
    saveRDS(this_survey, file = file.path(export_path, x), version = 2)
    x
  }

  if (is.null(export_path)) {
    # Results to memory  ---------------------------------
    lapply(subsetted_surveys, rename_survey_to_memory)
  } else {
    # Results to file  -----------------------------------

    vapply(subsetted_surveys, rename_survey_to_file, character(1))
  }
}
