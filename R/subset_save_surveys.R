#' Subset and Save Surveys
#' 
#' Read a predefined survey list and variables. 
#' 
#' @param var_harmonization Metadata of surveys, including at least
#' \code{filename}, \code{var_name_orig}, \code{var_name},
#'  \code{var_label}.
#' @param selection_name An identifier for the survey subset.
#' @param import_path The path to the survey files.
#' @param export_path The path where the subsets should be saved.
#' @importFrom dplyr distinct mutate 
#' @importFrom fs file_exists path_ext
#' @importFrom tidyselect all_of
#' @importFrom rlang set_names
#' @importFrom utils object.size
#' @importFrom fs dir_exists
#' @return The function does not return a value. It saves the subsetted
#' surveys into .rds files.
#' @export
#' @family import functions
#' @examples
#' \donttest{
#' test_survey <- read_rds (
#'  file = system.file("examples", "ZA7576.rds",
#'                     package = "retroharmonize")
#' )
#'
#' test_metadata <- metadata_create ( test_survey )
#' test_metadata <- test_metadata[c(18:37),]
#' test_metadata$var_name  <- var_label_normalize (test_metadata$var_name_orig)
#' test_metadata$var_label <- test_metadata$label_orig
#'
#' saveRDS(test_survey, file.path(tempdir(), 
#'                               "ZA7576.rds"), 
#'        version = 2)
#'
#' subset_save_surveys  ( var_harmonization = test_metadata, 
#'                       selection_name = "tested",
#'                       import_path = tempdir(), 
#'                       export_path = tempdir())
#'
#' file.exists ( file.path(tempdir(), "ZA7576_tested.rds"))
#' }

subset_save_surveys  <- function ( var_harmonization, 
                                   selection_name = "trust",
                                   import_path = "", 
                                   export_path = "working") {
  
  filename <- id <- var_name_orig <- var_label <- var_name <- NULL

  assertthat::assert_that(fs::dir_exists(import_path) == TRUE)

  selection <- var_harmonization %>%
    distinct (filename, id, var_name_orig, var_name, var_label ) 
  
  survey_files <- selection %>%
    distinct ( filename, id )
  
  for (i in 1:length(survey_files$filename) ) {
    this_file <- file.path(import_path, survey_files$filename[i])

    if ( ! fs::file_exists(this_file) ) {
      warning( this_file, " does not exist.")
      next
    }
    
    this_ext <- fs::path_ext(this_file)
    
    if ( this_ext %in% c("sav", "por")) {
      this_survey <- read_spss(this_file, id = survey_files$id[i])
    } else if (this_ext == "rds") {
      this_survey <- read_rds(file = this_file, id = survey_files$id[i])
    } else {
      ## add stata here
      next
    }
    
    survey_vars <- selection %>% 
      filter ( filename == this_file ) %>%
      select ( all_of(c("filename", "var_name_orig", "var_label")))
    
    new_names <- as.character(survey_vars$var_label_std)
    new_names <- gsub("-", "_", new_names)
    
    save_survey <- this_survey %>%
      select ( all_of (survey_vars$var_name_orig) ) %>%
      rlang::set_names( nm = new_names )
    
    save_file_name <- paste0(survey_files$id[i], "_", 
                             selection_name, ".rds")
    
    message ( "Saving ", save_file_name )
    
    saveRDS(save_survey, file = file.path(
      export_path, save_file_name ), 
      version = 2)
  }
}
