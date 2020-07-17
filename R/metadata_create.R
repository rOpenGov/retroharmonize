#' Create a metadata table
#' 
#' @param survey A survey data frame.
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate
#' @importFrom labelled na_values na_range val_labels var_label
#' @examples
#' metadata_create (
#'  survey = read_rds (
#'           system.file("examples", "ZA7576.rds",
#'                       package = "retroharmonize")
#'           )
#' )
#' @export

metadata_create <- function( survey ) {
  
  label_orig <- as.character(sapply ( survey, labelled::var_label))
  
  metadata <- tibble::tibble (
    var_name_orig = names(survey), 
    class_orig =  as.character(sapply( survey, function(x) class(x)[1])), 
    label_orig = ifelse ( vapply(label_orig, is.null, logical(1)), 
                          "", 
                          unlist(label_orig)) %>%
      as.character() %>%
      var_label_normalize()
  )
  
  val_labels_orig <- sapply ( survey, labelled::val_labels )
  
  value_labels_df <- data.frame (
    var_name_orig = names ( val_labels_orig  )
  )
  value_labels_df$labels <- as.character(val_labels_orig)
  value_labels_df$na_values <- sapply ( survey, labelled::na_values)
  value_labels_df$na_range <-  sapply ( survey, labelled::na_range )
  
  
  number_na_values <- function (x) {
    l <- length(unique(labelled::na_values(x)))
    ifelse (is.null(x), 
            0, 
            l)}
  
  number_valid_values <- function (x) {
    n <- length(unique(labelled::val_labels(x)))
    l <- length(unique(labelled::na_values(x)))
    
    ifelse (is.null(x), 
            0, 
            n-ifelse ( is.null(l), 0, l))}
  
  value_labels_df$n_na_values  <- lapply (survey, number_na_values)
  value_labels_df$n_cat_values <- lapply (survey, number_valid_values)
  
  metadata %>%
    dplyr::left_join ( value_labels_df, 
                       by = "var_name_orig") 
}
