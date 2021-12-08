#' @title Harmonize survey variables
#' 
#' @description Similar to \code{\link{subset_surveys}}, but will not only remove the 
#' variables that cannot be harmonized, but renames the remaining variables. 
#' 
#' @return A list of surveys or save individual rds files on the \code{export_path}.
#' @inheritParams subset_surveys
#' @export

harmonize_survey_variables <- function( crosswalk_table, 
                                        subset_name = "subset",
                                        survey_list = NULL,
                                        survey_paths = NULL,
                                        import_path = NULL, 
                                        export_path = NULL ) {
  
  ## This is a wrapper for subset_save_survey with strict validation of new variable names.
  
  is.crosswalk_table(crosswalk_table)
  
  ## selection: relevant metadata for this particular survey
  selection <- crosswalk_table %>% 
    distinct_all()
  
  ## metadata for the harmonization of variable names 
  crosswalk_var_names <- selection %>%
    select  ( all_of(c("id", "var_name_orig", "var_name_target")) ) %>%
    distinct_all() %>%
    group_by ( .data$id ) %>%
    dplyr::add_count (.data$var_name_target)
  
  multiple_target_names <- crosswalk_var_names$var_name_target[which(crosswalk_var_names$n>1)]
  multiple_target_names <- paste(multiple_target_names, collapse = ", ")
  
  assertthat::assert_that(
    # There should be no unambigous names, duplicates are not allowed.
    nchar(multiple_target_names)==0,
    msg = glue("The following names are duplicated in {survey_id}: {multiple_target_names}")
  )
  
  subset_surveys(crosswalk_table = crosswalk_table, 
                 subset_name = subset_name, 
                 survey_list = survey_list,
                 import_path  = import_path,
                 export_path = export_path )
  
}
