#' @title Create a codebook
#' 
#' @description Create a metadata table from the survey data files.
#' 
#' @param metadata A metadata table created by \code{\link{metadata_create}}.
#' @param survey A survey data frame, defaults to \code{NULL}. If the survey is
#' given as parameter, the metadata will be set to the metadata of this particular
#' survey by \code{\link{metadata_create}}.
#' @return A codebook for the survey as a data frame, including the metadata, and all found 
#' SPSS-type valid or missing labels. 
#' @importFrom assertthat assert_that
#' @importFrom tidyr unnest_longer
#' @importFrom dplyr mutate filter all_of arrange left_join bind_rows
#' @importFrom rlang set_names
#' @examples 
#' codebook_create (
#'  survey = read_rds (
#'           system.file("examples", "ZA7576.rds",
#'                       package = "retroharmonize")
#'           )
#' )
#' @export

codebook_create <- function ( metadata, 
                              survey = NULL) {
  
  if (!is.null(survey) ) {
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
  
  names( metadata)
  
  valid_labels <-  metadata %>%
    filter ( grepl( "spss", .data$class_orig )) %>%
    select ( all_of(c("entry", "id", "filename", "var_name_orig", "valid_labels")))   %>%
    unnest_longer( .data$valid_labels) %>%
    rlang::set_names ( c("entry", "id", "filename", "var_name_orig", "val_code_orig", "val_label_orig")) %>%
    mutate ( label_range = "valid")  
  
  na_labels <-  metadata %>%
    filter ( grepl( "spss", .data$class_orig )) %>%
    select ( all_of(c("entry", "id", "filename", "var_name_orig", "na_labels"))) %>%
    unnest_longer( .data$na_labels) %>%
    purrr::set_names ( c("entry", "id", "filename", "var_name_orig", "val_code_orig", "val_label_orig")) %>%
    mutate ( label_range = "missing") %>%
    filter ( !is.na(.data$val_code_orig))
  
  valid_labels %>%
    bind_rows (
      na_labels 
    ) %>%
    dplyr::arrange( .data$entry, .data$val_code_orig ) %>%
    left_join ( metadata %>% select ( any_of(c("entry", "id", "filename", "na_range", 
                                               "n_labels", "n_valid_labels", "n_missing_labels", 
                                               user_names))), 
                by = c("entry", "id", "filename"))
}





