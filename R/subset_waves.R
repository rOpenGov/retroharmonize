#' Subset all surveys in a wave
#' 
#' The function harmonizes the variable names of surveys (of class \code{survey}) that 
#' are imported from an external file as a wave with with \code{\link{read_surveys}}.
#' 
#' It is likely that you want to harmonize the variable names with \code{\link{harmonize_var_labels}} first.
#' 
#' @param waves A list of surveys imported with \code{\link{read_surveys}}.
#' @param subset_names The names of the variables that should be kept from all surveys in the list that contains the
#' wave of surveys. Defaults to \code{NULL} in which case it returns all variables without subsetting.
#' @importFrom dplyr select
#' @importFrom tidyselect any_of
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
#' hnw <- harmonize_var_names(waves = example_surveys, 
#'                            metadata = metadata )
#'                            
#' subset_waves (hnw, subset_names = c("uniqid", "w1", "age_education"))
#' @export


subset_waves <- function ( waves, 
                           subset_names = NULL ) {
  
  if ( is.null(subset_names) ) {
    return(waves)
  }
  
  subset_survey <- function(this_survey) {
    
    this_survey %>% select ( any_of ( subset_names ) )
  }
  
  lapply ( waves, subset_survey )
  
}
