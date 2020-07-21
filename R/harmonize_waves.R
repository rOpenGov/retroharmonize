#' Harmonize waves
#' 
#' Harmonize the values of surveys. Not fully functional yet. 
#' 
#' @param waves A list of surveys
#' @param .f A function
#' @export
#' @importFrom dplyr select
#' @importFrom tidyselect all_of

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
  do.call(rbind, tmp )

}