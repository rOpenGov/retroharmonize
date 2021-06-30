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
#' @param remove Defaults to \code{NULL}.  A character or regex that will be removed from all
#' old value labels, like \code{"\\("|\\)} for ( and ).
#' @param perl Use perl-like regex? Defaults to {FALSE}.
#' @importFrom labelled to_character labelled na_values val_labels
#' @importFrom labelled var_label
#' @importFrom tibble tibble 
#' @importFrom dplyr mutate left_join distinct select if_else
#' @importFrom tidyselect all_of
#' @importFrom haven labelled_spss
#' @importFrom assertthat assert_that
#' @importFrom rlang set_names
#' @family variable label harmonization functions
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
  name_orig = NULL, 
  remove = NULL,
  perl = FALSE ) {
  
  input_na_values <- na_values
  
  
  validate_label_list <- function ( label_list ) {
    
    ll_lengths <- vapply ( label_list, length, numeric(1))
    
    assertthat::assert_that(
      length(unique (ll_lengths))==1, 
      msg = paste0 ("The label_list elements must be of equal length, currently it is: ", 
                    as.character( paste(ll_lengths, collapse = ", ")))
    )
    
    ll <- as_tibble ( label_list )
    
    for ( l in unique( label_list$to) ) {
      
      matched_numeric_value <-  ll %>%
        filter ( .data$to == l ) %>%
        distinct ( .data$to, .data$numeric_values ) %>%
        pull ( .data$numeric_values )
      
      assert_that(length(matched_numeric_value)==1, 
                  msg = paste0("in harmonized_list ", 
                               l, " is matched with multiple numeric values: <", 
                               paste(matched_numeric_value, collapse = ",") , ">")
      )
      
    }
  }
  
  if (!is.null(harmonize_labels)) validate_label_list(harmonize_labels)
  
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
  
  if (is.null(harmonize_label)) harmonize_label <- labelled::var_label(x)
  if (is.null(harmonize_label)) harmonize_label <- original_x_name
  
  if ( !is.null(harmonize_labels)) {
    harmonize_labels <- validate_harmonize_labels(harmonize_labels)  ## see below main function
  }
  
  original_values <- tibble::tibble (
    x = vctrs::vec_data(x) )
  
  original_values$orig_labels <- if_else (
    condition = original_values$x %in% labelled::val_labels(x), 
    true = as_character(x), 
    false = NA_character_
  )
  
  if (!is.null(remove)) {
    harmonize_labels$from <- gsub(remove, "", harmonize_labels$from)
    original_values$orig_labels <- gsub(remove, "", original_values$orig_labels)
  }

  
  if (is.na_range_to_values(x)) {
    x <- na_range_to_values(x)
  }

  if (! is.null(harmonize_labels) ) {
    
    original_values$orig_labels <- if_else ( 
      #label == "" if not in the harmonization list
      condition = grepl(paste ( tolower(harmonize_labels$from), collapse = "|"), 
                   tolower(original_values$orig_labels), perl = perl), 
      true = tolower(original_values$orig_labels), 
      false  = "") 
    
    code_table <- dplyr::distinct_all(original_values)
    str <- code_table$orig_labels  #string of original values
    code_table$new_labels <- NA_character_
    code_table$new_values <- NA_real_
    
    for ( o in seq_along (harmonize_labels$from )) { 
      code_table$new_labels [which ( grepl ( tolower(harmonize_labels$from[o]), str))] <- harmonize_labels$to[o]
      code_table$new_values [which ( grepl ( tolower(harmonize_labels$from[o]), str))] <- harmonize_labels$numeric_values[o]
      }
    
    old_code <- function () {
      
      ## Can be removed after extensive testing -------
      for ( r in seq_along (harmonize_labels$to) ) {
        ## harmonize the strings to new labelling by regex
        str [which(grepl ( tolower(harmonize_labels$from[r]), str, perl = perl))] <- harmonize_labels$to[r]
      }
    
      add_new_values <- harmonize_labels %>%
        as_tibble() %>%
        dplyr::select ( tidyselect::all_of(
          c("to", "numeric_values")) ) %>%
        rlang::set_names( c("new_labels", "new_values"))
      
      code_table <- dplyr::left_join ( 
        code_table, 
        add_new_values, 
        by = "new_labels") %>%
        filter ( !is.na(.data$new_values) )
    }
    
    code_table$original_values <- NULL
    } else {
      
      # no harmonization is given -----------------------
      code_table <- get_labelled_attributes(x) ## see below main function
      na_labels <- names(na_values)
      
      if ( length(na_labels)>0 ) {
        # if there is no valid label harmonization, still check for potential missings
        potential_na_values <- sapply (na_labels, function(x) paste0("^", x,"|",x))
        na_regex <- sapply ( potential_na_values, function(s) grepl(s, val_label_normalize(code_table$new_labels), perl = perl))
        for (c in seq_along(na_labels) ){
          code_table$new_labels[which( na_regex[,c])] <- na_labels[c]
        }
      }
    }
  
  code_table <- code_table %>% distinct_all %>% dplyr::arrange(.data$new_values )
  
  new_value_table <- original_values %>%
    dplyr::left_join (
      code_table, 
      by = c("x", "orig_labels")) %>%
    dplyr::mutate ( new_values = if_else (
      condition = is.na(.data$new_labels),
      true = x, 
      false = .data$new_values
    )) %>%  #invalid labels should be treated elsewhere 
    dplyr::arrange( .data$new_values )
  
  ## define new missing values, not with range
  new_na_values <- new_value_table$new_values[which(new_value_table$new_values >= 99900 )]
  new_na_values <- unique(new_na_values)
  new_na_values <- union(input_na_values, new_na_values)
  
  # define new value - label pairs
  new_labelling <- new_value_table %>%
    dplyr::distinct ( .data$new_values, .data$new_labels ) 
  new_labels <- new_labelling$new_values
  names (new_labels) <- new_labelling$new_labels
  
  #define original labelling 
  original_labelling <- new_value_table %>%
        dplyr::distinct ( .data$new_values, .data$orig_labels )
  
  original_labels <- original_labelling$new_values
  names(original_labels) <- original_labelling$orig_labels
  
  # define original numeric code
  original_numerics <-  new_value_table %>%
    dplyr::distinct ( .data$new_values, .data$x )
  original_numeric_values <- original_numerics$new_values
  names(original_numeric_values) <- original_numerics$x
  
  # create new numerics
  new_numerics <- 
    tibble::tibble( x = original_values$x ) %>%
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

  new_total_labels2 <- new_total_labels[unique(names(new_total_labels))]

  attr(return_value, "labels") <- new_total_labels2[!is.na(new_total_labels2)]
  attr(return_value, "na_values") <- unique(new_na_values)
  attr(return_value, paste0(attr(return_value, "id"), "_values")) <- original_numeric_values
  
  assertthat::assert_that ( 
    inherits(return_value, "haven_labelled_spss") ) 
    
  return_value
  
}

#' @importFrom dplyr distinct_all
#' @keywords internal
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

                                    