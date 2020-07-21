#' Merge waves
#' 
#' Merge a list of surveys into a list with harmonized variable names,
#' variable labels and survey identifiers.
#' 
#' @param waves A list of surveys
#' @param var_harmonization Metadata of surveys, including at least
#' \code{filename}, \code{var_name_orig}, \code{var_name}, \code{var_label}.
#' @return A list of surveys with harmonized names and labels.
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr select mutate_if filter
#' @importFrom haven is.labelled
#' @importFrom tidyselect all_of
#' @family harmonization functions
#' @seealso survey

merge_waves <- function(waves, var_harmonization ) {
  
  if (any( ! c("filename", "var_name_orig", "var_name", "var_label") %in% names(var_harmonization))) {
    stop ( "var_harmonization must contain ", 
           paste(c("filename", "var_name_orig", "var_name", "var_label"), collapse = ", "), 
           ".")
  }
  
  fn_merge <- function(dat) {
    
    select_vars <- var_harmonization  %>% 
      dplyr::filter ( filename == attr(dat, "filename") )
    
    tmp <- dat %>%
      dplyr::select ( all_of (c("rowid", select_vars$var_name_orig)) ) %>%
      stats::setNames(., nm = select_vars$var_name ) 
    
    tmp <- tmp %>%
      mutate_if ( haven::is.labelled, 
                  ~as_labelled_spss_survey(., id = attr(tmp, "id"))) 

    if (! is.null(select_vars$var_label)) {
      labelled_items <- sapply ( tmp, is.labelled_spss_survey)
      labelled_items <- names(labelled_items)[labelled_items]
      
      labelling <- select_vars %>% 
        select (all_of(c("var_name", "var_label")))
      
      fn_relabelling <- function(x) {
        labelling$var_label[which(labelling$var_name == x)]
      }
      
      for ( x in labelled_items ) {
        attr(tmp[[x]], "label") <- fn_relabelling(x)
      } 
    }
    
    tmp
  }
  
  lapply(waves, fn_merge)
  
}
