#' Harmonize the values and labels of labelled vectors
#' 
#' @param x A labelled vector
#' @param harmonize_label A character vector of 1L containing the new,
#' harmonize variable label. Defaults to \code{NULL}, in which case 
#' it uses the variable label of \code{x}, unless it is also \code{NULL}.
#' @param harmonize_labels A list of harmonization values
#' @param na_values A named vector of \code{na_values}, the 
#' observations that are defined to be treated as missing in 
#' the SPSS-style coding.
#' @param na_range A min, max range of  \code{na_range}, the 
#' continuous missing value range. In most surveys this should be left 
#' \code{NULL}.
#' @param  name_orig The original name of the variable. If left \code{NULL}
#' it uses the latest name of the object \code{x}.
#' @param id A survey ID, defaults to \code{survey_id}
#' @importFrom labelled to_character labelled na_values val_labels
#' @importFrom labelled var_label
#' @importFrom tibble tibble 
#' @importFrom dplyr mutate left_join distinct select
#' @importFrom tidyselect all_of
#' @importFrom haven labelled_spss
#' @importFrom assertthat assert_that
#' @family harmonization functions
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
  harmonize_label  = NULL,
  harmonize_labels = NULL, 
  na_values = c("do_not_know" = 99997, 
                "declined" = 99998, 
                "inap" = 99999), 
  na_range = NULL,
  id = "survey_id",
  name_orig = NULL ) {
  
  new_values <- NULL
  input_na_values <- na_values

  if (is.null(id)) { 
    # if not otherwise stated, inherit the ID of x, if present
    if (!is.null(attr(x, "id"))) {
      id <- attr(x, "id")
    } else {
      id <- "unknown" 
    }
  }
  
  ## Get the original object name for recording it as metadata
  original_x_name <- deparse(substitute(x))  
  
  ## Set a label, if it is present but not given.
  if (is.null(harmonize_label)) { 
    harmonize_label <- labelled::var_label(x)
    }
  
  if ( !is.null(harmonize_labels)) {
    harmonize_labels <- validate_harmonize_labels(harmonize_labels)  ## see below main function
  }
  
  original_x <- vctrs::vec_data(x)

  original_values <- tibble::tibble (
    x = original_x)

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
    
    add_new_values <- harmonize_labels %>%
      dplyr::select ( tidyselect::all_of(
        c("to", "numeric_values")) ) %>%
      stats::setNames( c("new_labels", "new_values"))
    
    code_table <- dplyr::left_join ( code_table, 
                              add_new_values, 
                              by = "new_labels")
    
    code_table$original_values <- NULL
    } else {
      
      # no harmonization is given -----------------------
      code_table <- get_labelled_attributes(x) ## see below main function
      na_labels <- names(na_values)
      
      if ( length(na_labels)>0 ) {
        # if there is no valid label harmonization, still check for potential missings
        potential_na_values <- sapply (na_labels, function(x) paste0("^", x,"|",x))
        na_regex <- sapply ( potential_na_values, function(s) grepl(s, val_label_normalize(code_table$new_labels)))
        for (c in 1:length(na_labels)){
          code_table$new_labels[which( na_regex[,c])] <- na_labels[c]
        }
      }
    }
  
  code_table <- dplyr::arrange(.data =code_table, new_values )
  
  new_value_table <- original_values %>%
    dplyr::left_join (
      code_table, 
      by = c("x", "orig_labels")) %>%
    dplyr::mutate ( new_values = ifelse ( is.na(new_values), 
                                          99901, 
                                          new_values )) %>%
    dplyr::mutate ( new_labels = ifelse( new_values == 99901, 
                                         "invalid_label", 
                                         new_labels)) %>%
    dplyr::arrange( new_values )
  
  ## define new missing values, not with range
  new_na_values <- new_value_table$new_values[which(new_value_table$new_values >= 99900 )]
  new_na_values <- unique(new_na_values)
  new_na_values <- union(input_na_values, new_na_values)
  
  
  # define new value - label pairs
  new_labelling <- new_value_table %>%
    dplyr::distinct ( new_values, new_labels ) 
  new_labels <- new_labelling$new_values
  names (new_labels) <- new_labelling$new_labels
  
  #define original labelling 
  original_labelling <- new_value_table %>%
        dplyr::distinct ( .data$new_values, .data$orig_labels )
  
  original_labels <- original_labelling$new_values
  names(original_labels) <- original_labelling$orig_labels
  
  # define original numeric code
  original_numerics <-  new_value_table %>%
    dplyr::distinct ( new_values, x )
  original_numeric_values <- original_numerics$new_values
  names(original_numeric_values) <- original_numerics$x
  
  # create new numerics
  new_numerics <- 
    tibble::tibble( x = original_x) %>%
    dplyr::left_join (original_numerics, by = 'x' )

  return_value <- labelled_spss_survey(
    x = new_numerics$new_values,
    labels = labelled::val_labels(x),
    label = harmonize_label,
    na_values = labelled::na_values(x), 
    na_range = labelled::na_range(x), 
    id = id, 
    name_orig = original_x_name )
  
  
  add_to_value_range <- (!harmonize_labels$to %in% new_labelling$new_labels)
  
  further_labels <- harmonize_labels$numeric_values[add_to_value_range ]
  names(further_labels) <- harmonize_labels$to[add_to_value_range ]
  
  new_total_labels <- sort(c( new_labels, further_labels,
                              input_na_values[which( !input_na_values %in% new_labelling$new_labels)] ))

  attr(return_value, "labels") <- new_total_labels
  attr(return_value, "na_values") <- new_na_values
  attr(return_value, paste0(attr(return_value, "id"), "_values")) <- original_numeric_values
  
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

#' @importFrom dplyr distinct_all
get_labelled_attributes <- function(x) {
  
  unlabelled_x <- sapply(attributes(x), function(i) { attributes(i) <- NULL; x })[,1]

  code_table <- tibble::tibble (
    x = unlabelled_x,
    new_values = unlabelled_x
  ) 

  code_table$orig_labels <- as_character(x)  
  code_table$new_labels <- as_character(x)  
  
  dplyr::distinct_at(code_table, dplyr::vars(
    all_of(c("x", "new_values", "orig_labels", "new_labels"))))
}
