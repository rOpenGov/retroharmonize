#' Harmonize waves
#' 
#' Harmonize the values of surveys. It binds together variables
#' that are all present in the surveys, and applies a 
#' harmonization function on them. 
#' 
#' @param waves A list of surveys
#' @param .f A function to apply for the harmonization.
#' @export
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @family harmonization functions

harmonize_waves <- function(waves, .f) {
  
  joint_names <- names(waves[[1]])
  
  for ( i in 2:length(waves)) {
    joint_names <- intersect(names(waves[[i]]), joint_names)
  }
  
  joint_names 
  
  fn_subset <- function(dat) {
    dat %>% select (all_of(joint_names))
  }
  tmp <- lapply(waves, fn_subset)
  
  dat <- tmp[[1]]
  
  fn_harmonize <- function(dat, .f) {
    
    dat %>%
      mutate_if (is.labelled_spss_survey, .f )
  }
  
  tmp <- lapply ( tmp, function(x) fn_harmonize(x, .f) )
  
  do.call(rbind, tmp)
  

}