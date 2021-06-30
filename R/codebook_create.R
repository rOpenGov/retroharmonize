#' @title Create a codebook
#' 
#' @description Create a codebook from one or more survey data files.
#' 
#' @details For a list of survey waves, use \code{codebook_waves_create}.
#' The returned codebook contains only labelled variables, i.e., numeric and
#' character types are not included, because they do not require coding.
#' 
#' @param metadata A metadata table created by \code{\link{metadata_create}}. Defaults to \code{NULL}.
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
#' codebook_create (
#'  survey = read_rds (
#'           system.file("examples", "ZA7576.rds",
#'                       package = "retroharmonize")
#'           )
#' )
#' @export

codebook_create <- function ( metadata = NULL, 
                              survey = NULL) {
  
  assertthat::assert_that(
    ! ( is.null(metadata) & is.null(survey)), 
    msg = "Either the parameter 'metadata' or the parameter 'survey' must be given. They are both NULL."
  )
  
  if ( !is.null(metadata)) {
    if ( inherits ( metadata, "survey") & is.null(survey)) {
      survey = metadata
    }
  }

  if ( !is.null(survey) ) {
    assert_that ( inherits ( survey, "survey"), 
                  msg = "Parameter survey must be of class survey.")
    metadata <- metadata_create(survey) 
    }
  
  metadata_names <- c('filename', 'id', 'var_name_orig', 'class_orig', 'label_orig', 
                      'labels', 'valid_labels', 'na_labels', 'na_range', 
                      'n_labels', 'n_valid_labels', 'n_na_labels')
  
  user_names <- names(metadata)[ !names(metadata) %in% metadata_names ]
  
  assertthat::assert_that(
    all ( metadata_names %in% names(metadata ))
  )
  
  metadata$entry <- 1:nrow(metadata)
  
  change_label_to_empty <- function() {
    #moved to metadata_create
    "none" = NA_real_
   
  }
  
  metadata$label_type <- vapply(metadata$labels, function(x) class(x)[1], character(1))
  metadata$labels <- ifelse (metadata$label_type == "character" & metadata$n_labels ==0 , 
                             yes = change_label_to_empty(), 
                             no =  metadata$labels )
  metadata$valid_labels <- ifelse (metadata$label_type == "character" & metadata$n_labels ==0 , 
                                   yes = change_label_to_empty(), 
                                   no =  metadata$valid_labels )
  vapply(metadata$valid_labels, function(x) class(x)[1], character(1))
  metadata$na_labels <- ifelse (metadata$label_type == "character" & metadata$n_labels == 0 , 
                                yes = change_label_to_empty(), 
                                no =  metadata$na_labels )

  all( vapply(metadata$na_labels, function(x) class(x)[1], character(1)) == "numeric")
  
  char_labels <- vapply(metadata$na_labels, function(x) class(x)[1], character(1)) == "character"
  num_labels <- vapply(metadata$na_labels, function(x) class(x)[1], character(1)) == "numeric"

  valid_num_labels <-  metadata[num_labels ,] %>%
    filter ( grepl( "labelled", .data$class_orig )) %>%
    select ( all_of(c("entry", "id", "filename", "var_name_orig", "label_orig", "valid_labels")))   %>%
    unnest_longer( .data$valid_labels) %>%
    rlang::set_names ( c("entry", "id", "filename", "var_name_orig","label_orig",  "val_code_orig", "val_label_orig")) %>%
    mutate ( label_range = "valid", 
             val_code_orig = as.character(.data$val_code_orig))  
  
  valid_char_labels <-  metadata[char_labels ,] %>%
    filter ( grepl( "labelled", .data$class_orig )) %>%
    select ( all_of(c("entry", "id", "filename", "var_name_orig", "label_orig", "valid_labels")))   %>%
    unnest_longer( .data$valid_labels) %>%
    rlang::set_names ( c("entry", "id", "filename", "var_name_orig","label_orig",  "val_code_orig", "val_label_orig")) %>%
    mutate ( label_range = "valid")  %>%
    mutate ( val_code_orig = as.character(.data$val_code_orig) )
  
  na_num_labels <-  metadata[num_labels ,] %>%
    filter ( grepl( "labelled", .data$class_orig )) %>%
    select ( all_of(c("entry", "id", "filename", "var_name_orig", "label_orig",  "na_labels"))) %>%
    unnest_longer( .data$na_labels) %>%
    purrr::set_names ( c("entry", "id", "filename", "var_name_orig", "label_orig",  "val_code_orig", "val_label_orig")) %>%
    mutate ( label_range = "missing") %>%
    filter ( !is.na(.data$val_code_orig)) %>%
    mutate ( val_code_orig = as.character(.data$val_code_orig) )
  
  na_char_labels <-  metadata[char_labels ,] %>%
    filter ( grepl( "labelled", .data$class_orig )) %>%
    select ( all_of(c("entry", "id", "filename", "var_name_orig", "label_orig",  "na_labels"))) %>%
    unnest_longer( .data$na_labels) %>%
    purrr::set_names ( c("entry", "id", "filename", "var_name_orig", "label_orig",  "val_code_orig", "val_label_orig")) %>%
    mutate ( label_range = "missing") %>%
    filter ( !is.na(.data$val_code_orig)) %>%
    mutate ( val_code_orig = as.character(.data$val_code_orig) )
  
  num_labels <- valid_num_labels %>% 
    dplyr::bind_rows (
      na_num_labels 
    ) %>%
    dplyr::arrange( .data$entry, .data$val_code_orig ) %>%
    left_join ( metadata %>% select ( any_of(c("entry", "id", "filename", "na_range", 
                                               "n_labels", "n_valid_labels", "n_na_labels", 
                                               user_names))), 
                by = c("entry", "id", "filename"))
  
  if ( nrow ( na_char_labels) + nrow(na_num_labels) > 0 ) {
    char_labels <- valid_char_labels %>% 
      dplyr::bind_rows (
        na_char_labels 
      ) %>%
      dplyr::arrange( .data$entry, .data$val_code_orig ) %>%
      left_join ( metadata %>% select ( any_of(c("entry", "id", "filename", "na_range", 
                                                 "n_labels", "n_valid_labels", "n_na_labels", 
                                                 user_names))), 
                  by = c("entry", "id", "filename"))
    
    bind_rows ( num_labels, char_labels ) %>%
      dplyr::arrange (.data$entry)
    
  } else {
    num_labels %>%
      dplyr::arrange (.data$entry)
  }
}

#' @rdname codebook_create
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
#' codebook_waves_create (xample_surveys, to_harmonize )
#' }
#' @export
codebook_waves_create <- function ( waves ) {
  
  assertthat::assert_that( inherits(waves, "list"), 
                           msg = "The parameter waves must be a list (of surveys.)")
  
  
  assertthat::assert_that( all(unlist (lapply ( waves, function(x) inherits (x, "survey") ))), 
                           msg = "Every elements of the wave list must be of type survey.")
  
  codebook_list <- lapply ( waves, function(x) codebook_create (survey = x))
  
  do.call ( rbind, codebook_list )
}


