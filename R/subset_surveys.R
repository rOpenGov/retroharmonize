#' @title Subset all surveys in a wave
#' 
#' @description The function harmonizes the variable names of surveys (of class \code{survey}) that 
#' are imported from an external file as a wave with with \code{\link{read_surveys}}.
#' 
#' @details It is likely that you want to harmonize the variable names with 
#' \code{\link{harmonize_var_names}} first.
#' 
#' @param survey_list A list of surveys imported with \code{\link{read_surveys}}.
#' @param rowid The unique row (observation) identifier in the files. Defaults to 
#' \code{"rowid"}, which is the default of the importing functions in this package.
#' @param subset_names The names of the variables that should be kept from all surveys in the list that contains the
#' wave of surveys. Defaults to \code{NULL} in which case it returns all variables without subsetting.
#' @importFrom dplyr select
#' @importFrom tidyselect any_of
#' @return The list of surveys with harmonized variable names.
#' @examples
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)

#' metadata <- metadata_surveys_create(example_surveys)
#' 
#' metadata$var_name_suggested <- label_normalize(metadata$var_name)
#' 
#' metadata$var_name_suggested[metadata$label_orig == "age education"] <- "age_education"
#' 
#' hnw <- harmonize_var_names(survey_list = example_surveys, 
#'                            metadata    = metadata )
#'                            
#' subset_surveys (hnw, subset_names = c("uniqid", "w1", "age_education"))
#' @export

subset_surveys <- function ( survey_list, 
                             rowid = "rowid",
                             subset_names = NULL ) {
  
  if ( is.null(subset_names) ) {
    return(survey_list)
  }
  
  subset_names <- unique(c(rowid, subset_names))

  subset_survey <- function(this_survey) {
    
    this_survey %>% select ( any_of ( subset_names ) )
  }
  
  lapply ( survey_list, subset_survey )
  
}


#' @rdname subset_surveys
#' @param waves A list of surveys imported with \code{\link{read_surveys}}.
#' @export
subset_waves <- function( waves, subset_names = NULL) {
  
  .Deprecated(new = "subset_surveys", msg = "subset_waves is deprecated, use subset_surveys instead.")
  subset_surveys ( survey_list = waves, subset_names = subset_names )  
  
}