#' @title Create a codebook
#' 
#' @description Create a codebook from one or more survey data files.
#' 
#' @details For a list of survey waves, use \code{codebook_waves_create}.
#' The returned codebook contains only labelled variables, i.e., numeric and
#' character types are not included, because they do not require coding.
#' 
#' @param metadata A metadata table created by \code{\link{metadata_create}}. 
#' Defaults to \code{NULL}.
#' @param survey A survey data frame, defaults to \code{NULL}. If the survey is
#' given as parameter, the metadata will be set to the metadata of this particular
#' survey by \code{\link{metadata_create}}.
#' @return A codebook for the survey as a data frame, including the metadata, and all found 
#' SPSS-type valid or missing labels. 
#' @importFrom assertthat assert_that
#' @importFrom tidyr unnest_longer
#' @importFrom dplyr mutate filter all_of arrange left_join bind_rows
#' @importFrom rlang set_names
#' @family metadata functions
#' @examples 
#' create_codebook (
#'  survey = read_rds (
#'           system.file("examples", "ZA7576.rds",
#'                       package = "retroharmonize")
#'           )
#' )
#' @export

create_codebook <- function ( metadata = NULL, 
                              survey = NULL) {
  
  assertthat::assert_that(
    ! ( is.null(metadata) & is.null(survey)), 
    msg = "Either the parameter 'metadata' or the parameter 'survey' must be given. They are both NULL."
  )
  
  if ( !is.null(metadata)) {
    if ( inherits ( metadata, "survey") & is.null(survey)) {
      survey <- metadata
      warning("You did not give a survey parameter, but the metadata parameter was of type survey. It was treated as survey.")
    }
  }
  
  if ( !is.null(survey) ) {
    assert_that ( inherits ( survey, "survey"), 
                  msg = "Parameter survey must be of class survey.")
    metadata <- metadata_create(survey) 
  }
  
  metadata_names <- c('filename', 'id', 'var_name_orig', 'class_orig', 'var_label_orig', 
                      'labels', 'valid_labels', 'na_labels', 'na_range', 
                      'n_labels', 'n_valid_labels', 'n_na_labels')
  
  user_vars <- names(metadata)[!names(metadata) %in% metadata_names ]
  
  assertthat::assert_that(
    all ( metadata_names %in% names(metadata ))
  )
  
  metadata$entry <- 1:nrow(metadata)
  
  if ( length(user_vars)>0 ) {
    user_data <- metadata %>% select (all_of(c("entry", user_vars)))
  }
  
  
  metadata$label_type <- vapply(metadata$labels, function(x) class(x)[1], character(1))
  metadata$label_type <- ifelse ( 
    test = is.na(metadata$valid_labels)&is.na(metadata$na_labels), 
    yes  = "not_labelled", 
    no   = metadata$label_type)
  

  char_labels <- vapply(metadata$valid_labels, function(x) class(x)[1], character(1)) == "character"
  num_labels  <- vapply(metadata$valid_labels, function(x) class(x)[1], character(1)) == "numeric"
  
  metadata_labelled_numeric <- metadata[num_labels ,] 
  n_labelled_numeric  <-  ifelse ( !is.null(metadata_labelled_numeric), 
                                    nrow(metadata_labelled_numeric), 
                                    0) 
  
  if ( n_labelled_numeric  > 0 ) {
    # These area cases when the labels are of class numeric
    valid_labelled_numeric <-  metadata_labelled_numeric %>%
      filter ( grepl( "labelled", class_orig )) %>%
      select ( all_of(c("entry", "id", "filename", "var_name_orig", "var_label_orig", "valid_labels")))   %>%
      unnest_longer( valid_labels) %>%
      rlang::set_names ( c("entry", "id", "filename", "var_name_orig","var_label_orig",  "val_code_orig", "val_label_orig")) %>%
      mutate ( 
        # This is the valid observation range
        label_range = "valid", 
        val_code_orig = as.character(val_code_orig))  
    
    na_labelled_numeric <-  metadata[num_labels ,] %>%
      filter ( grepl( "labelled", class_orig )) %>%
      select ( all_of(c("entry", "id", "filename", "var_name_orig", "var_label_orig",  "na_labels"))) %>%
      unnest_longer( na_labels) %>%
      purrr::set_names ( c("entry", "id", "filename", "var_name_orig", "var_label_orig",  "val_code_orig", "val_label_orig")) %>%
      mutate ( 
        # This is the missing observation range
        label_range = "missing") %>%
      filter ( !is.na(val_code_orig) ) %>%
      mutate ( val_code_orig = as.character(val_code_orig) )
    
    
    num_labels <- valid_labelled_numeric  %>% 
      dplyr::bind_rows (
        na_labelled_numeric 
      ) %>%
      dplyr::arrange( entry, val_code_orig ) %>%
      left_join ( metadata %>% select ( any_of(c("entry", "id", "filename", "na_range", 
                                                 "n_labels", "n_valid_labels", "n_na_labels", 
                                                 user_vars))), 
                  by = c("entry", "id", "filename"))
    
   
  }
  
  metadata_labelled_character <- metadata[char_labels ,] 
  n_labelled_character <- ifelse ( 
    !is.null(metadata_labelled_character), 
    nrow(metadata_labelled_character), 
    0) 
  
  if ( n_labelled_character > 0) {
    # These area cases when the na_labels are of class character
    valid_labelled_character  <-   metadata_labelled_character %>%
      filter ( grepl( "labelled", class_orig )) %>%
      select ( all_of(c("entry", "id", "filename", "var_name_orig", "var_label_orig", "valid_labels")))   %>%
      unnest_longer( valid_labels) %>%
      rlang::set_names ( c("entry", "id", "filename", "var_name_orig","var_label_orig",  "val_code_orig", "val_label_orig")) %>%
      mutate ( 
        # This is the valid observation range
        label_range = "valid")  %>%
      mutate ( val_code_orig = as.character(val_code_orig) )
    
    
    na_labelled_character <-  metadata[char_labels ,] %>%
      filter ( grepl( "labelled", class_orig )) %>%
      select ( all_of(c("entry", "id", "filename", "var_name_orig", "var_label_orig",  "na_labels"))) %>%
      unnest_longer( na_labels) %>%
      purrr::set_names ( c("entry", "id", "filename", "var_name_orig", "var_label_orig",  
                           "val_code_orig", "val_label_orig")) %>%
      mutate ( 
        # This is the missing observation range
        label_range = "missing") %>%
      filter ( !is.na(val_code_orig)) %>%
      mutate ( val_code_orig = as.character(val_code_orig) )
    
    
    char_labels <- valid_labelled_character %>% 
      dplyr::bind_rows (
        na_labelled_character 
      ) %>%
      dplyr::arrange( entry, val_code_orig ) %>%
      left_join ( metadata %>% select ( any_of(c("entry", "id", "filename", "na_range", 
                                                 "n_labels", "n_valid_labels", "n_na_labels", 
                                                 user_vars))), 
                  by = c("entry", "id", "filename"))
    
  }

  if ( n_labelled_character + n_labelled_numeric == 0 ) {
    # There are no labelled variables
    tibble ( entry = vector("integer"), 
             id = vector("character"),
             filename = vector("character"), 
             var_name_orig = vector("character"), 
             var_label_orig = vector("character"), 
             val_code_orig = vector("character"), 
             val_label_orig = vector("character"), 
             label_range = vector("character"), 
             na_range = vector("character"), 
             n_labels = vector("numeric"),
             n_valid_labels = vector("numeric"), 
             n_na_labels = vector("numeric")) %>%
      left_join ( user_data[0,], by = "entry" )
  } else if ( n_labelled_character == 0 ) {
    num_labels %>%
      dplyr::arrange (entry)
  } else if ( n_labelled_numeric == 0  ) {
    char_labels %>%
      dplyr::arrange (entry)
  } else {
   num_labels %>%
      bind_rows ( char_labels) %>%
      dplyr::arrange (entry)
  }
}

#' @rdname create_codebook
#' @param waves A list of surveys. 
#' @export
codebook_waves_create <- function ( waves ) {
  .Deprecated(new = 'codebook_surveys_create', 
              msg = "codebook_waves_create() is deprecated, use codebook_surveys_create() instead.")
}

#' @rdname create_codebook
#' @param survey_list A list containing surveys of class survey.
#' @family metadata functions
#' @examples
#' \donttest{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)     
#' 
#' codebook_surveys_create (example_surveys)
#' }
#' @export

codebook_surveys_create <- function ( survey_list ) {
  
  assertthat::assert_that( inherits(survey_list, "list"), 
                           msg = "The parameter waves must be a list (of surveys.)")
  
  assertthat::assert_that( all(unlist (lapply ( survey_list, function(x) inherits (x, "survey") ))), 
                           msg = "Every elements of the wave list must be of type survey.")
  
  codebook_list <- lapply ( survey_list, function(x) create_codebook (survey = x))
  
  do.call ( rbind, codebook_list )
}


