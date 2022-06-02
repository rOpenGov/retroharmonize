#' @title Create a metadata table
#' 
#' @description Create a metadata table from the survey data files.
#' 
#' @details A data frame like tibble object is returned. 
#' In case you are working with several surveys, a list of surveys or a vector
#' of file names containing the full path to the survey must be called with 
#' \code{\link{metadata_create}}, which is a wrapper around 
#' a list of  \code{\link{metadata_survey_create}} calls.
#' 
#' The structure of the returned tibble:
#' \describe{
#'   \item{filename}{The original file name; if present; \code{missing}, if a non-\code{\link{survey}} data frame is used as input \code{survey}.}
#'   \item{id}{The ID of the survey, if present; \code{missing}, if a non-\code{\link{survey}} data frame is used as input \code{survey}.}
#'   \item{var_name_orig}{The original variable name in SPSS.}
#'   \item{class_orig}{The original variable class after importing with\code{\link[haven]{read_spss}}.}
#'   \item{var_label_orig}{The original variable label in SPSS.}
#'   \item{labels}{A list of the value labels.}
#'   \item{valid_labels}{A list of the value labels that are not marked as missing values.}
#'   \item{na_labels}{A list of the value labels that refer to user-defined missing values.}
#'   \item{na_range}{An optional range of a continuous missing range, if present in the vector.}
#'   \item{n_labels}{Number of categories or unique levels, which may be different from the sum of missing and category labels.}
#'   \item{n_valid_labels}{Number of categories in the non-missing range.}
#'   \item{n_na_labels}{Number of categories of the variable, should be the sum of the former two.}
#'   \item{na_levels}{A list of the user-defined missing values.}
#' }
#' 
#' @param survey A survey data frame. You receive a survey object with any importing function, i.e. 
#' \code{\link{read_rds}}, \code{\link{read_spss}} \code{\link{read_dta}}, \code{\link{read_csv}} or 
#' their common wrapper \code{\link{read_survey}}.
#' You can construct it with \code{\link{survey}} from a data frame, too. 
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate case_when group_by ungroup
#' @importFrom tidyr nest unnest
#' @importFrom labelled na_values na_range val_labels var_label
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @family metadata functions
#' @return A nested data frame with metadata and the range of 
#' labels, na_values and the na_range itself.
#' @examples
#' metadata_create (
#'  survey_list = read_rds (
#'                    system.file("examples", "ZA7576.rds",
#'                    package = "retroharmonize")
#'           )
#' )
#' @export

metadata_survey_create <- function(survey) {
  
  ## Assertions before running the function -----------------------------
  if ( "list" %in% class(survey) ) {
    assert_that(all(vapply ( survey, is.survey, logical(1))), 
                msg = "Parameter 'survey' is not of s3 class survey or a list of them. See ?is.survey.")
    metadata_df <- metadata_surveys_create(survey_list = survey)
    return(metadata_df)
  } else if ( 
    # Accidentally the file names were supplied.
    # This will validate if the surveys are indeed existing files.
    is.character(survey) ) {
    warning("The parameter 'survey' is not a single survey but a character vector. Try to understand them as a file names. See ?metadata_surveys_create.")
    metadata_df <- metadata_surveys_create(survey_files = survey)
    return(metadata_df)
  } else {
    assert_that(is.survey(survey), 
                msg = "Parameter 'survey' must be of s3 class survey. See ?is.survey.")
  }

  
  filename <- attr(survey, "filename")
  
  if (is.null(filename)) filename <- "unknown"
  id <- attr(survey, "id")
  if (is.null(id)) id <- "missing"
  
  if( ncol(survey) == 0) {
    # Special case when file could not be read and survey is empty
    return(metadata_initialize(filename = filename, id = paste0(filename, " could not be read.")))
  }
  
  var_label_orig  <- lapply (survey, labelled::var_label)
  
  class_orig <- vapply( survey, function(x) class(x)[1], character(1))
  
  metadata <- tibble::tibble (
    filename = filename, 
    id = id,
    var_name_orig = names(survey), 
    class_orig =  class_orig, 
    var_label_orig = ifelse ( vapply(var_label_orig, is.null, logical(1)), 
                          "", 
                          unlist(var_label_orig)) %>%
      as.character() %>%
      var_label_normalize()
  )
 
  fn_valid_range <- function(x) {
    labelled::val_labels(x)[!labelled::val_labels(x) %in% labelled::na_values(x)]
  }
  
  na_labels <- function(x) {
    # labels that refer to na_values
    labs <- labelled::val_labels(x)
    if ( is.null(labs)) return(NA_character_)
    selected_labs <- labelled::na_values(x)
    labs[ labs %in% selected_labs ]
  }
  
  to_list_column <- function(.f = "na_values") {
    
    # We use sapply because the length is to be discovered. 
    
    x <- case_when ( 
      .f == "na_labels" ~ sapply (survey, na_labels),  # internal function above
      .f == "na_range"  ~ sapply (survey, labelled::na_range), 
      .f == "valid_range"  ~ sapply (survey, fn_valid_range),    # internal function above
      .f == "labels" ~ sapply (survey, labelled::val_labels)
      )
    
    x[sapply(x, is.null)] <- NA_character_
    names(x) <- names(survey)
    x
  }
  
  range_df  <- tibble::tibble (
    var_name_orig = names(survey),
    labels = to_list_column(.f = "labels"),
    valid_labels = to_list_column(.f = "valid_range"),
    na_labels = to_list_column(.f = "na_labels"),
    na_range = to_list_column (.f = "na_range")
    )
  
  label_length <- function(x) {
    
    ifelse ( is.na(x[[1]])[1] | length(x[[1]]) ==0,
             0, length(x[[1]]) )
  }
  
  range_df$n_labels <- vapply(1:nrow(range_df), function(x) label_length(range_df$labels[x]), numeric(1))
  range_df$n_valid_labels <- vapply(1:nrow(range_df), function(x) label_length(range_df$valid_labels[x]), numeric(1))
  range_df$n_na_labels <- vapply(1:nrow(range_df), function(x) label_length(range_df$na_labels[x]), numeric(1))
  

  return_df <- metadata %>%
    left_join ( range_df %>% 
                  group_by ( .data$var_name_orig ) %>%
                  tidyr::nest() , 
                by = "var_name_orig") %>%
    tidyr::unnest ( cols = "data" )  %>%
    ungroup() %>%
    mutate ( n_na_labels = as.numeric(.data$n_na_labels), 
             n_valid_labels = as.numeric(.data$n_valid_labels), 
             n_labels = as.numeric(.data$n_labels)) %>%
    as.data.frame()
  
  change_label_to_empty <- function() {
    "none" = NA_real_
  }
  ## Avoid the accidental creation of empty CHARACTER lists, because they do not bind with 
  ## numeric lists. 
  
  return_df$label_type <- vapply(return_df$labels, function(x) class(x)[1], character(1))
  return_dflabels <- ifelse (return_df$label_type == "character" & return_df$n_labels ==0 , 
                             yes = change_label_to_empty(), 
                             no =  return_df$labels )
  return_df$valid_labels <- ifelse (return_df$label_type == "character" & return_df$n_labels ==0 , 
                                   yes = change_label_to_empty(), 
                                   no =  return_df$valid_labels )
  return_df$na_labels <- ifelse (return_df$label_type == "character" & return_df$n_labels == 0 , 
                                yes = change_label_to_empty(), 
                                no =  return_df$na_labels )
  
  return_df %>%
    select ( -.data$label_type )
}

#' @title Create a metadata table from several surveys
#' @rdname metadata_create
#' @param inheritParams read_surveys
#' @family metadata functions
#' @examples
#' examples_dir <- system.file( "examples", package = "retroharmonize")
#'
#' my_rds_files <- dir( examples_dir)[grepl(".rds", 
#'                                         dir(examples_dir))]
#'
#' example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))
#' metadata_create (example_surveys)
#' @export

metadata_create <- function ( survey_list = NULL, 
                              survey_paths = NULL, 
                              .f = NULL) {
 
  if ( !is.null(survey_list) ) {
    validate_survey_list(survey_list)
    if (! "list" %in% class(survey_list)) {
      assert_that(is.survey(survey_list), 
                  msg = "metadata_create(survey_list, ...) is neither a list nor a survey.")
      survey_id <-  attr(survey_list, "id")
      survey_list <- list ( i = survey_list )
      names(survey_list)[1] <-survey_id
    }
    metadata_list <- lapply ( survey_list, metadata_survey_create )
    do.call ( rbind, metadata_list )
  } else if (is.null(survey_paths)) {
    stop("Error in metadata_surveys_create(): both 'survey_list' and 'survey_paths' are NULL.")
  } else {
    validate_survey_files (survey_paths)
    read_survey_create_metadata <- function(x, .f) {
      tmp <- read_survey(x, .f)
      message ("Read: ", x)
      metadata_survey_create(tmp)
    }
    
    metadata_list <- lapply ( X = survey_paths, 
                              FUN = function(x) read_survey_create_metadata(x, .f) )
    do.call(rbind, metadata_list)
  }
}

#' @rdname metadata_create
#' @details The form \code{metadata_waves_create} is deprecated.

metadata_waves_create <- function(survey_list) {
  .Deprecated(new = "metadata_surveys_create",
              msg = "metadata_waves_create() is deprecated, use create_surveys_metadata() instead", 
              old = "merge_waves")
  metadata_surveys_create(survey_list)
  }

#' @title Initialize a metadata data frame
#'
#' @inheritParams metadata_create
#' @importFrom tibble tibble
#' @return A nested data frame with metadata and the range of 
#' labels, na_values and the na_range itself.
#' @keywords internal
metadata_initialize <- function (filename, id ){
  
  tibble ( 
    filename = filename, 
    id = id, 
    class_orig = NA_character_,
    var_name_orig = NA_character_, 
    var_label_orig   = NA_character_, 
    labels       = NA_character_, 
    valid_labels = list("none" = NA_real_),
    na_labels = list("none" = NA_real_),
    na_range = list("none" = NA_real_),
    n_labels = 0,
    n_valid_labels = 0,
    n_na_labels = 0 )  
  
}


