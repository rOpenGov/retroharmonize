#' @title Crosswalk surveys
#' 
#' @description Harmonize surveys with crosswalk tables. 
#' 
#' @details Harmonize a survey or a list of surveys with the help of a crosswalk table.
#' You can create the crosswalk table with \code{\link{crosswalk_table_create}}, or manually 
#' create a crosswalk table as a data frame including at least the following columns: \code{id}
#' for identifying a survey, \code{var_name_orig} for the original variable name 
#' and \code{var_name_target} for the new (target) variable name. Optionally you can harmonize
#' the value labels, the numeric codes, and the special missing labels, too.
#' 
#' @param survey_list A list of \code{\link{survey}} objects.
#' @param crosswalk_table A table created with \code{crosswalk_table_create}, or a 
#' data frame with at least the following columns: \code{var_name_orig}, \code{var_name_target},
#' for harmonizing the variable names. If \code{val_label_orig}, \code{val_label_target} 
#' are present, the value labels will be harmonized, too. If 
#' \code{var_numeric_orig}, \code{var_numeric_target} are present, the numeric codes of the 
#' variable will be harmonized. If \code{var_class} is present, then the class of the variable 
#' will be harmonized to any of \code{factor}, \code{numeric} or \code{character} using 
#' \code{\link{as_factor}}, \code{\link{as_numeric}}, or \code{\link{as_character}}.
#' @param na_values A named vector of \code{na_values}, the 
#' observations that are defined to be treated as missing in 
#' the SPSS-style coding. Defaults to \code{NULL}.
#' @importFrom dplyr filter select mutate distinct_all relocate across everything
#' @importFrom rlang .data 
#' @examples 
#' \donttest{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)
#' 
#' ## Compare with documentation:
#' documented_surveys <- metadata_surveys_create(example_surveys)
#' documented_surveys <- documented_surveys[
#' documented_surveys$var_name_orig %in% c( "rowid", "isocntry", "w1", "qd3_4",
#'                                           "qd3_8" , "qd7.4", "qd7.8", "qd6.4", "qd6.8"),
#'                                           ]
#' crosswalk_table    <- crosswalk_table_create ( metadata = documented_surveys )
#' }
#' @importFrom dplyr relocate
#' @export

crosswalk_surveys <- function(survey_list, crosswalk_table, na_values = NULL ) {
  
  assertthat::assert_that(is.null(na_values)|is.character(na_values)|is.numeric(na_values), 
                          msg = "Parameter 'na_values' must be a named character or numeric string.")
  
  set_na_values <- na_values
  
  relabel_survey <- function(y, selection) {
    
    assertthat::assert_that(inherits(selection, 'data.frame'), 
                            msg = "selectin must be a data.frame")
    
    assertthat::assert_that(nrow(selection)>0, 
                            msg = "selection must have rows")

    select_to_harmonize <- selection %>%
      filter ( !is.na(.data$val_label_orig) )
    
    vars_to_harmonize <- unique(select_to_harmonize$var_name_target) 
    
    if ( length(vars_to_harmonize) == 0 ) return (y)
    
    return_value <- y
    
    for ( this_var in vars_to_harmonize ) {
 
      correspondence_table <- select_to_harmonize %>%
        filter ( .data$var_name_target == this_var )
      
      assertthat::assert_that(is.numeric(correspondence_table$val_numeric_target), 
                              msg = "Error in relabel_survey: 'val_numeric_target' must be a numeric vector")
      
      
      harmonize_these_labels <- function(z) {
        harmonize_labels = list ( 
          from = paste0("^", correspondence_table$val_label_orig, "$"), 
          to =   correspondence_table$val_label_target, 
          numeric_values = correspondence_table$val_numeric_target
          )
        
        harmonize_values (x = z, 
                          harmonize_labels = harmonize_labels, 
                          na_values = set_na_values) 
      } 
      
    
      return_value <- return_value %>%
        mutate ( across(any_of(this_var), harmonize_these_labels ))
    }
    
    return_value
  }
  
  subset_survey <- function(this_survey) {
    
    survey_id <- NULL
    survey_id <- attr(this_survey, "id")
    if ( is.null(survey_id)) survey_id <- "survey_id"
    
    if ( survey_id %in% unique(crosswalk_table$id) ) {
      selection <- crosswalk_table %>% 
        filter ( .data$id == survey_id ) %>%
        distinct_all()
      
      crosswalk_var_names <- selection %>%
        select  ( all_of(c("var_name_orig", "var_name_target")) ) %>%
        distinct_all()

      ## First harmonize the variable names 
      tmp <- this_survey %>% 
        select ( all_of(crosswalk_var_names$var_name_orig) ) %>%
        set_names( crosswalk_var_names$var_name_target ) %>%
        mutate ( id = attr(this_survey, "id")) %>%
        relocate ( .data$id, .before = everything())

      ## Second harmonize labels if possible
      
      if (  # label harmonization is possible
        all ( c("val_label_orig", "val_label_target", "val_numeric_target") %in% names(selection) )
      ) {
        tmp <- relabel_survey( y = tmp, selection = selection )
      }
      
      
      ## At last harmonnize class if possible
        
      if ( ! "var_class" %in% names(selection) ) return(tmp)
      
      factor_vars <- selection$var_name_target[selection$var_class == "factor"]
      character_vars <- selection$var_name_target[selection$var_class == "character"]
      numeric_vars <-selection$var_name_target[selection$var_class == "numeric"]
      
      return_df <-  tmp %>%
        mutate ( across (any_of(factor_vars), as_factor), 
                 across (any_of(character_vars), as_character), 
                 across (any_of(numeric_vars), as_numeric)
                 )
      
      return_df 
    } else {
      message (
        glue::glue("{survey_id} is not present in 'survey_list'")
      )
    }
  }
  
  subsetted <- lapply ( survey_list, function(x) subset_survey(x) )
  
  subsetted
  
}

crosswalk <- function(survey_list, crosswalk_table, na_values = NULL) {
  
  crosswalked <-  crosswalk_surveys(
    survey_list = survey_list, 
    crosswalk_table = crosswalk_table, 
    na_values = na_values )
  
  if ( ! is.null(crosswalked) & length(crosswalked)>1 ) {
    do.call(rbind, crosswalked )
  } else {
    crosswalked
  }
}

#' @title Create a crosswalk table
#' 
#' @description Create a crosswalk table with the source variable names and variable labels.
#' 
#' @details The table contains a \code{var_name_target} and \code{val_label_target} column, but 
#' these values need to be set by further manual or reproducible harmonization steps.
#' 
#' @param metadata A metadata table created by \code{\link{metadata_create}}.
#' @return A tibble with raw crosswalk table. It contains all harmonization tasks, but the 
#' target values need to be set by further manipulations. 
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @export

crosswalk_table_create <- function( metadata ) {
  
  assertthat::assert_that(inherits(metadata, "data.frame"), 
                          msg = "Parameter 'metadata' must be a data frame object.")
  
  assertthat::assert_that(nrow(metadata)>=1, 
                          msg = "The 'metadata' data frame must have at least one row.")
  
  compulsory_columns <- c("filename", "id", "var_name_orig", "labels")
  missing_columns <- compulsory_columns[! which(compulsory_columns %in% names(metadata))]
  
  assertthat::assert_that(
    length(missing_columns)==0, 
    msg = glue::glue("The 'metadata' data frame has missing columns: {paste(missing_columns, collapse = ", "}")
  )
  
  fn_labels <- function(x) {
    
    if ( is.na(x$labels) ) {
      # The variable is not labelled
      label_length <- 1
      tibble (
        id = rep(unique(x$id), label_length),
        filename = rep(unique(x$filename), label_length),
        var_name_orig = rep(x$var_name_orig, label_length),
        var_name_target = rep(x$var_name_orig, label_length),
        val_numeric_orig = NA_real_,
        val_numeric_target  = NA_real_,
        val_label_orig = NA_character_,
        val_label_target = NA_character_
      )
    } else {
      val_labels <- names(x$labels)
      label_length <- length(unlist(x$labels))
      
      tibble (
        id = rep(unique(x$id), label_length),
        filename = rep(unique(x$filename), label_length),
        var_name_orig = rep(x$var_name_orig, label_length),
        var_name_target = rep(x$var_name_orig, label_length),
        val_numeric_orig = as.numeric(unlist(x$labels)),
        val_numeric_target  = as.numeric(unlist(x$labels)),
        val_label_orig = as.character(vapply ( x$labels, names, character(label_length) )),
        val_label_target = as.character(vapply ( x$labels, names, character(label_length) ))
      )
    }
  }
  
  if (nrow(metadata)==1) {
    fn_labels(metadata[1,])
  } else {
    do.call (rbind, lapply ( 1:nrow(metadata), function(x) fn_labels(metadata[x,])  ))
  }
}
