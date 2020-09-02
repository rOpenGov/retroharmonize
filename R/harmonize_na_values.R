#' Harmonize na_values in haven_labelled_spss
#' 
#' @param df A data frame that contains haven_labelled_spss vectors.
#' @return A tibble where the na_values are consistent
#' @importFrom labelled to_character labelled na_values val_labels
#' @importFrom tibble deframe
#' @importFrom dplyr case_when
#' @examples
#' \donttest{
#' examples_dir <- system.file( "examples", package = "retroharmonize")
#' 
#' test_read <- read_rds ( 
#'      file.path(examples_dir, "ZA7576.rds"),
#'      id = "ZA7576", 
#'      doi = "test_doi")
#' 
#' harmonize_na_values(test_read)
#' }
#' @export
#' @family harmonization functions

harmonize_na_values <- function(df) {
  
  missing_value_labels <- NULL
  
  # should be paramteric
  
  vars_with_spss_missings <- vapply ( df, is.labelled_spss, logical(1) )
  
  ## If there are no haven_labelled_spss in the tibble, return it ----
  if ( ! any(vars_with_spss_missings) ) return(df)
  
  vars_with_spss_missings <- names(df)[which(vars_with_spss_missings)]
  
  tibble::deframe ( df[,i])
  for ( i in vars_with_spss_missings ) {
    
    this_var <- tibble::deframe (df[, i])
    summary (this_var)
    these_values <-  unique ( this_var )
    
    these_labels <- labelled::val_labels(this_var)
    user_na <- labelled::na_values(this_var)
    not_missing_labels <- these_labels [!these_labels %in% user_na ]
    missing_values <- these_labels [which ( these_labels %in% user_na )]  
    h_missing_value_labels <- tolower(names(missing_values))
    
    h_missing_value_labels <- dplyr::case_when (
      grepl ( "^inap", h_missing_value_labels) ~ "inap", 
      grepl ( "^declin", h_missing_value_labels) ~ "declined", 
      grepl ( "^na", h_missing_value_labels) ~ "missing",
      grepl ( "^dk", h_missing_value_labels) ~ "do_not_know",
      grepl ( "do_not", h_missing_value_labels) ~ "do_not_know",
      grepl ( "_inap", h_missing_value_labels) ~ "inap",
      grepl ( "_declin", h_missing_value_labels) ~ "declined",
      TRUE ~ paste0("<missing>_", h_missing_value_labels)
    )
    
    for ( m in length(missing_value_labels)) {
      change_label <- which( names(these_labels) == names(missing_values[m]) )
      names(these_labels)[change_label] <- h_missing_value_labels[m] 
    }
    harmonized <- labelled::labelled(unclass(this_var), these_labels)
    new_labels <- labelled::to_character(harmonized)
    
    harmonized_missing_labels <- vector(
      "numeric", 
      length = length(h_missing_value_labels))
    
    harmonized_missing_labels <- dplyr::case_when (
      grepl ( "inap", h_missing_value_labels) ~ 99999, 
      grepl ( "^declin", h_missing_value_labels) ~ 99998, 
      grepl ( "^na", h_missing_value_labels) ~ 99996,
      grepl ( "^do_not", h_missing_value_labels) ~ 99996,
      grepl ( "^<missing>", h_missing_value_labels) ~ 99901
    ) 
    
    harmonized_numeric_value <- dplyr::case_when (
      grepl ( "inap", new_labels) ~ 99999, 
      grepl ( "^declin", new_labels) ~ 99998, 
      grepl ( "^na", new_labels) ~ 99996,
      grepl ( "^do_not", new_labels) ~ 99996,
      grepl ( "^<missing>", new_labels) ~ 99901,
      TRUE ~ unclass(harmonized)
    )

    harmonized_labels <- dplyr::case_when (
      grepl ( "inap", names(these_labels)) ~ 99999, 
      grepl ( "^declin", names(these_labels)) ~ 99998, 
      grepl ( "^na", names(these_labels)) ~ 99996,
      grepl ( "^do_not_know", names(these_labels)) ~ 99996,
      grepl ( "^<missing>", names(these_labels)) ~ 99901,
      TRUE ~ these_labels)
    
    names ( harmonized_labels ) <- names (these_labels )
    
    replace_var <- labelled::labelled(harmonized_numeric_value, 
                                      harmonized_labels)
    df[, i] <- replace_var
    
  }
  
  df
}
