#' Labelled to labelled_spss_survey
#' 
#' @param x A vector of class haven_labelled or haven_labelled_spss.
#' @param id The survey identifier.
#' @return A vector of labelled_spss_survey
#' @family type conversion functions
#' @export
 
as_labelled_spss_survey  <- function (x, id) {
  
  if ( ! haven::is.labelled(x)) {
    
    stop( deparse(substitute(x)), "must be haven_labelled or a class that inherits its labels.")
  }
  
   labelled_spss_survey(
    x = vctrs::vec_data(x), 
    labels = labelled::val_labels(x), 
    label = labelled::var_label(x),
    na_values = labelled::na_values(x), 
    na_range = labelled::na_range(x), 
    name_orig = deparse(substitute(x)), 
    id = id
  )
  
  
}