#' @title Harmonize the variable names of surveys
#' 
#' @description The function harmonizes the variable names of surveys (of class \code{survey}) that 
#' are imported from an external file as a wave.
#' 
#' @details If the \code{metadata} that contains subsetting information is subsetted, then 
#' it will subset the surveys in 
#' \code{waves}.
#' 
#' @param waves A list of surveys imported with \code{\link{read_surveys}}.
#' @param metadata A metadata table created by \code{metadata_create} and binded together for 
#' all surveys in \code{waves}.
#' @param old The column name in \code{metadata} that contains the old, not harmonized variable names.
#' @param new The column name in \code{metadata} that contains the new, harmonized variable names.
#' @param rowids Rename var labels of original vars \code{rowid} to simply \code{uniqid}?
#' @importFrom dplyr mutate left_join select inner_join
#' @importFrom tidyselect all_of
#' @importFrom rlang set_names
#' @importFrom assertthat assert_that
#' @importFrom glue glue
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
#' metadata$var_name_suggested <- label_normalize(metadata$var_name)
#' 
#' metadata$var_name_suggested[metadata$label_orig == "age education"] <- "age_education"
#' 
#' harmonize_var_names(waves = example_surveys, 
#'                     metadata = metadata )
#' @export


harmonize_var_names <- function ( waves, 
                                  metadata,
                                  old = "var_name_orig",
                                  new = "var_name_suggested",
                                  rowids = TRUE ) {

  assert_that( all(c(new, old, "filename") %in% names(metadata)), 
               msg = glue::glue("'{old}', '{new}' and 'filename' must be column names in metadata.")
  )
  
  metadata <- metadata %>%
    select ( all_of(c(old, new, "filename"))) %>%
    set_names ( c("var_name_orig", "var_name_suggested", "filename") )
 
      
  rename_wave <- function (this_survey) {
    
    this_metadata <- metadata[attr(this_survey, "filename") == metadata$filename, ]
    
    if ( ! attr(this_survey, "filename") %in% metadata$filename ) {
      warning (glue::glue("The metadata of {attr(this_survey, 'filename')} cannot be found") )
    }
    
    renaming <- data.frame ( var_name_orig = names(this_survey) ) %>%
      inner_join ( this_metadata %>% 
                    select ( all_of (c("var_name_orig", "var_name_suggested"))), 
                  by = "var_name_orig")
    
    subset_this_survey <- this_survey %>%
      select ( all_of (renaming$var_name_orig) ) 
    
    rlang::set_names(subset_this_survey, 
                     nm = renaming$var_name_suggested)
     
  }

 lapply ( waves, rename_wave )

}
