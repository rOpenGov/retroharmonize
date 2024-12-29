#' @keywords  internal
validate_survey_list <- function(survey_list) { 
  
  assert_that(!is.null(survey_list), 
              msg = "The parameter 'survey_list' is NULL.")

  assert_that("list" %in%  class (survey_list) | is.survey(survey_list), 
              msg = "The parameter 'survey_list' is not a list or a single survey.")
  
  if ( "list" %in%  class (survey_list) ) {
    are_these_surveys <- vapply ( survey_list, is.survey, logical(1))
    not_surveys <- paste( which (are_these_surveys == FALSE), collapse = ", ")
    assert_that(all(are_these_surveys), 
                msg = glue::glue("{not_surveys} in 'survey_list' are not surveys.") )
    
    n_survey <- length(survey_list)
    
    filenames <- vapply ( survey_list, function(x) attr(x, "filename"), character(1))
    ids <-  vapply ( survey_list, function(x) attr(x, "id"), character(1))
    
    duplicate_ids <- ids[duplicated (ids )]
    missing_ids <- vapply ( survey_list, function(x) is.null(attr(x, "id")), logical(1))
    missing_filenames <- vapply ( survey_list, function(x) is.null(attr(x, "filename")), logical(1))
    
    assert_that(! all(missing_ids), 
                msg = paste0(paste(which(missing_ids), " have no IDs"))
    )
    
    assert_that(! all(missing_filenames), 
                msg = paste0(paste(which(missing_ids), " have no filenames"))
    )
    
    ids  <- tryCatch({
      vapply ( survey_list, function(x) attr(x, "id"), character(1))
    }, 
    error = function(cond) {
      message ( "Some IDs are not character(1L) single characters.") 
    },
    finally = {}
    )
    
    filenames  <- tryCatch({
      vapply ( survey_list, function(x) attr(x, "filename"), character(1))
    }, 
    error = function(cond) {
      message ( "Some filenames are not character(1L) single characters.") 
    },
    finally = {}
    )
    
    duplicate_ids <- ids[duplicated (ids )]
    duplicate_filenames <- filenames[duplicated (filenames)]

    assert_that(length(duplicate_ids)==0, 
                msg = paste0(
                  paste(duplicate_ids, collapse = ", "), 
                  " are not unique."
                ))
    
    assert_that(length(duplicate_filenames)==0, 
                msg = paste0(
                  paste(duplicate_filenames, collapse = ", "), 
                  " are not unique."
                ))
  }
}

#' @importFrom fs file_exists
#' @keywords internal
validate_survey_files <- function(survey_files) {
  
  existing_survey_files <- vapply ( survey_files, fs::file_exists, logical(1))
  not_existing_survey_files <- existing_survey_files [ existing_survey_files == FALSE  ]
  
  if (length(not_existing_survey_files)>0) {
    error_msg <- paste0("the following files were not found :", paste(names(not_existing_survey_files), collapse = ", "))
    stop ("Error in validate_survey_files() -  ", error_msg)
  }
  
  TRUE
}

#' @title Validate harmonize_labels parameter
#' Check if "from", "to", and "numeric_values" are of equal lengths.
#' @importFrom dplyr select
#' @importFrom assertthat assert_that
#' @keywords  internal
validate_harmonize_labels <- function( harmonize_labels ) {
  
  if( inherits(harmonize_labels, "list") | inherits (harmonize_labels, "data.frame") ) {
    
    assertthat::assert_that(
      all(c("from", "numeric_values", "to") %in% names (harmonize_labels)), 
      msg = "The harmonize_values must contain <from>, <to> and <numeric_values> vectors."
    )
    
    assertthat::assert_that(
      inherits( harmonize_labels$numeric_values, "numeric"), 
      msg = "The harmonize_values must a numeric <numeric_values> vector."
    )
   
    assertthat::assert_that(is.numeric(harmonize_labels$numeric_values) |
                              is.null(harmonize_labels$numeric_values), 
                            msg = "The harmonize_values must have a numeric <numeric_values> with non-NULL or non-NA values.")
    
    assertthat::assert_that(
      inherits( harmonize_labels$from, "character"), 
      msg = "The harmonize_values must a character <from> vector."
    )
    
    assertthat::assert_that(
      inherits( harmonize_labels$to, "character"), 
      msg = "The harmonize_values must a character <to> vector."
    )
    
    list_length <- as.numeric(vapply ( c("from", "numeric_values", "to"), function(x) length(harmonize_labels[[x]]), numeric(1)))
    
    assertthat::assert_that(
      all(vapply ( list_length, function(x) list_length[[1]] == x, logical(1))), 
      msg = "<harmonize_label> must have <from>, <to>, <numeric_values> of equal lengths.")
      
    
  } else {
    stop("<harmonize label> must have <from>, <to>, <numeric_values> of equal lengths as list or data.frame.")
  }
}

