#' Harmonize the values and labels of labelled vectors
#' 
#' @param x A labelled vector
#' @param harmonize_labels A list of harmonization values
#' @param na_values A named vector of na_values.
#' @param id A survey ID, defaults to \code{survey_id}
#' @importFrom labelled to_character labelled na_values val_labels
#' @importFrom tibble tibble 
#' @importFrom dplyr mutate left_join distinct
#' @importFrom haven labelled_spss
#' @importFrom assertthat assert_that
#' @examples
#' var1 <- labelled::labelled_spss(x = c(1,0,1,1,0,8,9), 
#'                                 labels = c("TRUST" = 1, 
#'                                            "NOT TRUST" = 0, 
#'                                            "DON'T KNOW" = 8, 
#'                                            "INAP. HERE" = 9), 
#'                                 na_values = c(8,9))
#' 
#' harmonize_values (var1)                                
#' 
#' var2 <- labelled::labelled_spss(x = c(1,6,2,9,1,1,2), 
#'                                 labels = c("Tend to trust" = 1, 
#'                                            "Tend not to trust" = 2, 
#'                                            "DK" = 8, 
#'                                            "Inap" = 9), 
#'                                 na_values = c(6,9))
#' 
#' harmonize_values (var2)
#' 
#' var3 <- labelled::labelled(x = c(1,6,2,9,1,1,2), 
#'                            labels = c("Tend to trust" = 1, 
#'                                       "Tend not to trust" = 2, 
#'                                       "DK" = 6))
#' harmonize_values (var3)
#' @export
#' @family harmonization functions


harmonize_values <- function(x, 
                             harmonize_labels = list ( 
                               from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk", "^inap"), 
                               to = c("trust", "not_trust", "do_not_know", "inap"),
                               numeric_value = c(1,0,99997, 99999)
                             ), 
                             na_values = c("do_not_know", "inap"), 
                             id = "survey_id") {
  
  if (is.null(id)) attr(x, "id") <- "unknown"
  
  assertthat::assert_that(is.numeric(harmonize_labels$numeric_value) |
                is.null(harmonize_labels$numeric_value))

  harmonize_labels <- validate_harmonize_labels(harmonize_labels)  ## see below main function
  
  original_values <- tibble::tibble (
    orig_labels = labelled::to_character(x),
    x = unclass(x)
  )
  
  if (is.na_range_to_values(x)) {
    x <- na_range_to_values(x)
  }

  original_values$orig_labels <- ifelse ( 
    test = grepl(paste ( harmonize_labels$from, collapse = "|"), 
                 tolower(original_values$orig_labels)), 
    yes = tolower(original_values$orig_labels), 
    no  = "")
  
  original_values$new_labels <- ""
  
  these_value_labels <- unique(original_values$orig_labels)
  str <- these_value_labels

  for ( r in 1:length(harmonize_labels$to) ) {
    str [which(grepl ( harmonize_labels$from[r], str))] <- harmonize_labels$to[r]
  }

  for ( j in 1:length(these_value_labels)) {
    # create new value labels
    original_values$new_labels <- ifelse ( 
      original_values$orig_labels ==  these_value_labels[j], 
      str[j], 
      original_values$new_labels )
  }
  
  new_values <- original_values %>%
    dplyr::left_join (tibble::tibble ( 
      new_labels = harmonize_labels$to, 
      new_values = harmonize_labels$numeric_value), 
      by = 'new_labels') %>%
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
  new_labelling <- dplyr::distinct ( new_values, new_values, new_labels )
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
validate_harmonize_labels <- function( harmonize_list ) {
  if( inherits(harmonize_labels, "list") ) {
    if(!all(sort (names ( harmonize_labels )) == c("from", "numeric_value", "to"))) {
      stop( "<harmonize_label> must have <from>, <to>, <numeric_value> of equal lengths.")
    }
    if(length(unique(vapply(harmonize_labels, length, integer(1)))) !=1) {
      stop("<harmonize_label> must have <from>, <to>, <numeric_value> of equal lengths.")
    }
  } else if ( inherits(harmonize_labels, "data.frame") ) {
    if(!all(sort (names ( harmonize_labels )) == c("from", "numeric_value", "to"))) {
      stop( "<harmonize_label> must have <from>, <to>, <numeric_value>.")
    }
  } else {
    stop("<harmonize label> must have <from>, <to>, <numeric_value> of equal lengths as list or data.frame.")
  }
  harmonize_labels <- tibble::as_tibble(harmonize_labels)
  dplyr::select (harmonize_labels, 
                 tidyselect::all_of(c("from", "to", "numeric_value")))
}
