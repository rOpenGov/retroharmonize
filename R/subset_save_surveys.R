#' Subset and Save Surveys
#' 
#' Read a predefined survey list and variables. 
#' 
#' @param var_harmonization Metadata of surveys, including at least
#' \code{filename}, \code{var_name_orig}, \code{var_name}, \code{var_label}.
#' @param selection_name An identifier for the survey subset.
#' @param import_path The path to the survey files.
#' @param export_path The path where the subsets should be saved.
#' @importFrom dplyr distinct mutate 
#' @importFrom fs file_exists path_ext
#' @importFrom tidyselect all_of
#' @importFrom stats setNames
#' @importFrom utils object.size
#' @importFrom fs dir_exists
#' @return The function does not return a value. It saves the subsetted
#' surveys into .rds files.
#' @export
#' @family import functions
#' @examples
#' \dontrun{
#' ## See Eurobaromter case study
#' }

subset_save_surveys  <- function ( var_harmonization, 
                                   selection_name = "trust",
                                   import_path = "", 
                                   export_path = "working") {
  
  filename <- id <- var_name_orig <- var_label_std <- NULL

  assertthat::assert_that(fs::dir_exists(import_path) == TRUE)

  selection <- var_harmonization %>%
    distinct (filename, id, var_name_orig, var_label_std ) %>%
    mutate ( filename = file.path(import_path, filename)) 
  
  survey_files <- selection %>%
    distinct ( filename, id )
  
  for (i in 1:length(survey_files$filename)) {
    this_file <- survey_files$filename[i]

    if ( ! fs::file_exists(this_file) ) {
      warning( this_file, " does not exist.")
      next
    }
    
    this_ext <- fs::path_ext(this_file)
    
    if ( this_ext %in% c("sav", "por")) {
      this_survey <- read_spss(this_file, id = survey_files$id[i])
    } else if (this_ext == "rds") {
      this_survey <- read_rds(this_file, id = survey_files$id[i])
    } else {
      ## add stata here
      next
    }
    
    survey_vars <-selection %>% 
      filter ( filename == this_file ) %>%
      select ( all_of(c("filename", "var_name_orig", "var_label_std")))
    
    new_names <- as.character(survey_vars$var_label_std)
    new_names <- gsub("-", "_", new_names)
    
    save_survey <- this_survey %>%
      select ( all_of (survey_vars$var_name_orig) ) %>%
      stats::setNames( nm = new_names )
    
    save_file_name <- paste0(survey_files$id[i], "_", 
                             selection_name, ".rds")
    
    message ( "Saving ", save_file_name )
    
    saveRDS(save_survey, file = file.path(
      export_path, save_file_name ), 
      version = 2)
  }
}
