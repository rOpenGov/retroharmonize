#' Harmonize the variable names of surveys
#' 
#' The function harmonizes the variable names of surveys (of class \code{survey}) that 
#' are imported from an external file as a wave.
#' 
#' @param waves A list of surveys imported with \code{\link{read_surveys}}.
#' @param metadata A metadata table created by \code{metadata_create} and binded together for 
#' all surveys in \code{waves}.
#' @param rowids Rename var labels of original vars \code{rowid} to simply \code{uniqid}?
#' @importFrom dplyr mutate left_join select
#' @importFrom tidyselect all_of
#' @importFrom purrr set_names
#' @importFrom assertthat assert_that
#' @family harmonization functions
#' @return The list of surveys with harmonized variable names.
#' @examples
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)

#' metadata <- lapply ( X = example_surveys, FUN = metadata_create )
#' metadata <- do.call(rbind, metadata)
#' 
#' metadata$var_name <- label_normalize(metadata$var_name)
#' 
#' metadata$var_name [metadata$label_orig == "age education"] <- "age_education"
#' 
#' harmonize_var_names(waves = example_surveys, 
#'                     metadata = metadata )
#' @export


harmonize_var_names <- function ( waves, metadata, rowids = TRUE ) {
  
  if ( rowids == TRUE) {
    metadata <- metadata %>% 
      mutate ( var_name = ifelse ( .data$var_name_orig == "rowid", "uniqid", .data$var_name ) )
  }
  
  rename_wave <- function (this_survey) {
    
    this_metadata <- metadata[attr(this_survey, "filename") == metadata$filename, ]
    assertthat::assert_that(nrow(this_metadata)>2, 
                            msg = glue::glue("The metadata of {attr(this_survey, 'filename')} cannot be found")
    )
    
    renaming <- data.frame ( var_name_orig = names(this_survey) ) %>%
      left_join ( this_metadata %>% 
                    select ( all_of (c("var_name_orig", "var_name"))), 
                  by = "var_name_orig")
    
    purrr::set_names(this_survey, nm = renaming$var_name)
    
  }
  
  lapply ( waves, rename_wave )
}
