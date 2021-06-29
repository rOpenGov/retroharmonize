#' Merge waves
#' 
#' Merge a list of surveys into a list with harmonized variable names,
#' variable labels and survey identifiers.
#' 
#' @param waves A list of surveys
#' @param var_harmonization Metadata of surveys, including at least
#' \code{filename}, \code{var_name_orig}, \code{var_name}, \code{var_label}.
#' @return A list of surveys with harmonized names and variable labels.
#' @export
#' @importFrom rlang set_names
#' @importFrom dplyr select mutate_if filter
#' @importFrom haven is.labelled
#' @importFrom tidyselect all_of
#' @family survey harmonization functions
#' @seealso survey
#' @examples 
#' \donttest{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)
#'     
#' metadata <- lapply ( X = example_surveys, FUN = metadata_create )
#' metadata <- do.call(rbind, metadata)
#'  
#' to_harmonize <- metadata %>%
#'   dplyr::filter ( var_name_orig %in% 
#'                   c("rowid", "w1") |
#'                   grepl("trust ", label_orig ) ) %>%
#'   dplyr::mutate ( var_label = var_label_normalize(label_orig) ) %>%
#'   dplyr::mutate ( var_name = val_label_normalize(var_label) )
#' 
#' merge_waves ( example_surveys, to_harmonize )
#' }

merge_waves <- function(waves, var_harmonization) {
  
  . <- filename <- NULL
  
  validate_survey_list(waves)
  
  if (any( ! c("filename", "var_name_orig", "var_name", "var_label") %in% names(var_harmonization))) {
    stop ( "var_harmonization must contain ", 
           paste(c("filename", "var_name_orig", "var_name", "var_label"), collapse = ", "), 
           ".")
  }
  
  fn_merge <- function(dat) {
    
    select_vars <- var_harmonization  %>% 
      dplyr::filter ( filename == attr(dat, "filename") )
    
    if ( ! "rowid" %in% select_vars$var_name_orig ) {
      warning("rowid is not selected from ", attr(dat, "filename") )
    }
    
    tmp <- dat %>%
      dplyr::select ( all_of (c( select_vars$var_name_orig)) ) %>%
      rlang::set_names( nm = select_vars$var_name ) 
    
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
