

#' @keywords  internal
validate_survey_list <- function(survey_list) { 
  
  assert_that(is.list(survey_list))
  assert_that(is.survey(survey_list[[1]]))
  
  n_survey <- length(survey_list)
  
  filenames <-  sapply ( survey_list, function(x) attr(x, "filenames"))
  ids <-  sapply ( survey_list, function(x) attr(x, "ids"))
  
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
                paste(duplicate_ids), 
                " are not unique."
              ))
  
  assert_that(length(duplicate_filenames)==0, 
              msg = paste0(
                paste(duplicate_filenames), 
                " are not unique."
              ))
}

#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @keywords  internal
validate_harmonize_labels <- function( harmonize_labels ) {
  
  if( inherits(harmonize_labels, "list") ) {
    if ( "numeric_value" %in% names(harmonize_labels) ) {
      names(harmonize_labels)[which(names(harmonize_labels)=="numeric_value")] <- "numeric_values"
    }
    
    if( !all( 
      sort (names ( harmonize_labels )) 
      == c("from", "numeric_values", "to")) ) {
      stop( "<harmonize_label> must have <from>, <to>, <numeric_values> of equal lengths.")
    }
    
    if(length(
      unique(vapply(harmonize_labels, length, integer(1)))
    ) !=1) {
      stop("<harmonize_label> must have <from>, <to>, <numeric_values> of equal lengths.")
    }
  } else if ( inherits(harmonize_labels, "data.frame") ) {
    if(!all(sort (names ( harmonize_labels )) == c("from", "numeric_values", "to"))) {
      stop( "<harmonize_label> must have <from>, <to>, <numeric_values>.")
    }
  } else {
    stop("<harmonize label> must have <from>, <to>, <numeric_values> of equal lengths as list or data.frame.")
  }
  harmonize_labels <- tibble::as_tibble(harmonize_labels)
  
  assertthat::assert_that(is.numeric(harmonize_labels$numeric_values) |
                            is.null(harmonize_labels$numeric_values))
  
  dplyr::select (harmonize_labels, 
                 tidyselect::all_of(c("from", "to", "numeric_values")))
}