#' Harmonize the values and labels of labelled vectors
#' 
#' @param x A labelled vector
#' @param harmonize_labels A list of harmonization values
#' @param na_values A named vector of \code{na_values}, the 
#' observations that are defined to be treated as missing in 
#' the SPSS-style coding.
#' @param id A survey ID, defaults to \code{survey_id}
#' @importFrom labelled to_character labelled na_values val_labels
#' @importFrom tibble tibble 
#' @importFrom dplyr mutate left_join distinct
#' @importFrom haven labelled_spss
#' @importFrom assertthat assert_that
#' @return A labelled vector that contains in its metadata attributes
#' the original labelling, the original numeric coding and the current
#' labelling, with the numerical values representing the harmonized
#' coding.
#' @examples
#' var1 <- labelled::labelled_spss(
#'   x = c(1,0,1,1,0,8,9), 
#'   labels = c("TRUST" = 1, 
#'              "NOT TRUST" = 0, 
#'              "DON'T KNOW" = 8, 
#'              "INAP. HERE" = 9), 
#'   na_values = c(8,9))
#' 
#' harmonize_values (
#'   var1, 
#'   harmonize_labels = list ( 
#'     from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
#'     to = c("trust", "not_trust", "do_not_know", "inap"),
#'     numeric_values = c(1,0,99997, 99999)), 
#'     na_values = c("do_not_know" = 99997,
#'                 "inap" = 99999), 
#'     id = "survey_id"
#' )
#' @export
#' @family harmonization functions

harmonize_values <- function(
  x, 
  harmonize_labels = list ( 
    from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"),
    numeric_values = c(1,0,99997, 99999)
  ), 
  na_values = c("do_not_know" = 99997, 
                "declined" = 99998, 
                "inap" = 99999), 
  na_range = NULL,
  id = "survey_id") {
  
  if (is.null(id)) attr(x, "id") <- "unknown"
  
  if ( !is.null(harmonize_labels)) {
    harmonize_labels <- validate_harmonize_labels(harmonize_labels)  ## see below main function
  }
  
  original_values <- tibble::tibble (
    x = unclass(x))
  original_values$orig_labels = as_character(x)
  
  if (is.na_range_to_values(x)) {
    x <- na_range_to_values(x)
  }

  if (! is.null(harmonize_labels) ) {
    original_values$orig_labels <- ifelse ( 
      test = grepl(paste ( harmonize_labels$from, collapse = "|"), 
                   tolower(original_values$orig_labels)), 
      yes = tolower(original_values$orig_labels), 
      no  = "") 
    
    code_table <- dplyr::distinct_all(original_values)
    str <- code_table$orig_labels
    
    for ( r in 1:length(harmonize_labels$to) ) {
      ## harmonize the strings to new labelling by regex
      str [which(grepl ( harmonize_labels$from[r], str))] <- harmonize_labels$to[r]
    }
    
    code_table$new_labels <- str
    code_table$new_values <- harmonize_labels$numeric_values
    } else {
    code_table <- tibble::as_tibble(get_labelled_attributes(x))
    names(code_table) <- c("orig_labels","new_labels","new_values")
  }
  
  new_values <- original_values %>%
    dplyr::left_join (
      code_table, 
      by = c("x", "orig_labels")) %>%
    dplyr::mutate ( new_values = ifelse ( is.na(new_values), 
                                          99901, 
                                          new_values )) %>%
    dplyr::mutate ( new_labels = ifelse( new_values == 99901, 
                                         "invalid_label", 
                                         new_labels)) 
  
  ## define new missing values, not with range
  new_na_values <- new_values$new_values[which(new_values$new_values >= 99900 )]
  new_na_values <- unique(new_na_values)
  
  # define new value - label pairs
  new_labelling <- dplyr::distinct ( 
    new_values, new_values, new_labels )
  new_labels = new_labelling$new_values
  names (new_labels) <- new_labelling$new_labels
  
  #define original labelling 
  original_labelling <- new_values %>%
        dplyr::distinct ( .data$new_values, .data$orig_labels )
  
  original_labels <- original_labelling$new_values
  names(original_labels) <- original_labelling$orig_labels
  
  #define original numeric code
  original_numerics <- dplyr::distinct ( new_values, new_values, x )
  original_values <- original_numerics$new_values
  names(original_values) <- original_numerics$x
  
  # create a labelled_spss
  return_value <- haven::labelled_spss(new_values$new_values,
                                labels = sort ( new_labels ), 
                                na_values = new_na_values)
  
  
  attr(return_value, paste0(id, "_labels")) <-  labelled::val_labels(x) 
  attr(return_value, paste0(id, "_values")) <- original_values
  attr(return_value, "id") <- id
  
  assertthat::assert_that ( inherits(return_value, "haven_labelled_spss")) 
    
  return_value
  
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
    sort (names ( harmonize_labels )) == c("from", "numeric_values", "to")) ) {
      stop( "<harmonize_label> must have <from>, <to>, <numeric_values> of equal lengths.")
    }
    
  if(length(unique(vapply(harmonize_labels, length, integer(1)))) !=1) {
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


get_labelled_attributes <- function(x) {
  
  current_labels <- labelled::val_labels(x)
  
  list (
    from = names(current_labels), 
    to = names(current_labels), 
    numeric_values = as_numeric(unclass(current_labels))
  )
  
}