#' @title Harmonize survey variables
#' 
#' @description Similar to \code{\link{subset_surveys}}, but will not only remove the 
#' variables that cannot be harmonized, but renames the remaining variables. 
#' 
#' @return A list of surveys or save individual rds files on the \code{export_path}.
#' @importFrom dplyr left_join filter select distinct_all
#' @importFrom tibble tibble 
#' @importFrom rlang set_names
#' @inheritParams subset_surveys
#' @export

harmonize_survey_variables <- function( crosswalk_table, 
                                        subset_name = "subset",
                                        survey_list = NULL,
                                        survey_paths = NULL,
                                        import_path = NULL, 
                                        export_path = NULL ) {
  
  ## This is a wrapper for subset_save_survey with strict validation of new variable names.
  
  is.crosswalk_table(crosswalk_table)  ## validates structure and unambigous naming
  
  subsetted_surveys <-  subset_surveys(crosswalk_table = crosswalk_table, 
                                       subset_name = subset_name, 
                                       survey_list = survey_list,
                                       import_path = import_path,
                                       export_path = export_path )
  
  rename_survey_to_memory <- function(x) {
    
    if (is.survey(x)) { 
      this_survey <- x 
    } else if (is.character(x)) {
      this_survey <- read_rds(file.path(export_path, x))
    } else {
      stop (" rename_survey_file(x) x must be a filename or a survey.")
    }
    
    survey_id <- gsub(paste0("_", subset_name), "", attr(this_survey, "id"))
    
    new_names <- tibble( var_name_orig = names(x)) %>%
      left_join (
        crosswalk_table %>% 
          filter (.data$id == survey_id) %>% 
          select ( .data$var_name_orig, .data$var_name_target ) %>%
          distinct_all(), 
        by = "var_name_orig",
      ) %>% select ( .data$var_name_target ) %>% unlist() %>% as.character()
    
    rlang::set_names(x, nm = new_names )
      
  }
  
  rename_survey_to_file <- function(x) {
    
    if (is.survey(x)) { 
      this_survey <- x 
    } else if (is.character(x)) {
      this_survey <- read_rds(file.path(export_path, x))
    } else {
        stop (" rename_survey_file(x) x must be a filename or a survey.")
      }
    
    survey_id <- gsub(paste0("_", subset_name), "", attr(this_survey, "id"))
    
    new_names <- tibble( var_name_orig = names(this_survey)) %>%
      left_join (
        crosswalk_table %>% 
          filter (.data$id == survey_id) %>% 
          select ( .data$var_name_orig, .data$var_name_target ) %>%
          distinct_all(), 
        by = "var_name_orig",
      ) %>% select ( .data$var_name_target ) %>% unlist() %>% as.character()
    
    this_survey <- rlang::set_names(this_survey, nm = new_names )
    saveRDS(this_survey, file = file.path(export_path, x), version = 2 )
    x
  }
  
  if ( is.null(export_path) ) {
    # Results to memory  ---------------------------------
    lapply ( subsetted_surveys, rename_survey_to_memory )
  } else {
    # Results to file  -----------------------------------
    as.character(vapply ( subsetted_surveys, rename_survey_to_file, character(1) ))
  }
}


