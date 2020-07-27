## Internal functions not to be exported ----------------------

cat_line <- function(...) {
  # identical to haven:::cat_line
  cat(paste0(..., "\n", collapse = ""))
}

is.labelled_spss <- function (x) inherits(x, "haven_labelled_spss")
is.labelled <- function (x) inherits(x, "haven_labelled")

#' Convert to haven_labelled_spss
#' @param x A vector
#' @param na_labels A named vector of missing values, defaults to
#' \code{c( "inap" = "inap")} for character vectors and
#' \code{c( 99999 = "inap")} for numeric vectors.
#' @return A haven_labelled_spss vector
#' @importFrom labelled val_labels
#' @keywords internal

convert_to_labelled_spss <- function(x, na_labels = NULL ) {

  if ( is.null(na_labels) && is.numeric(unclass(x))) {
    na_labels <- structure(99999, names = "inap")
  } else if (is.null(na_labels) && is.character(unclass(x))) {
    na_labels <- structure("inap", names = "inap")
  }

  stopifnot( ! any(unclass(na_labels) %in% x) )


  labelled_spss(x, c( labelled::val_labels (x), na_labels),
                na_values = unclass(na_labels))
}

validate_survey_list <- function(survey_list) { 
  
  assert_that(is.list(survey_list))
  assert_that(is.survey(survey_list[[1]]))
  
  n_survey <- length(survey_list)
  
  filenames <-  sapply ( survey_list, function(x) attr(x, "filenames"))
  
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
