#' Create a metadata table
#' 
#' @param survey A survey data frame.
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate case_when group_by
#' @importFrom tidyr nest unnest
#' @importFrom labelled na_values na_range val_labels var_label
#' @importFrom purrr map
#' @return A nested data frame with metadata and the range of 
#' labels, na_values and the na_range itself.
#' @examples
#' metadata_create (
#'  survey = read_rds (
#'           system.file("examples", "ZA7576.rds",
#'                       package = "retroharmonize")
#'           )
#' )
#' @export

metadata_create <- function( survey ) {
  var_name_orig <- NULL
  
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
 
  fn_valid_range <- function(x) {
    labelled::val_labels(x)[!labelled::val_labels(x) %in% labelled::na_values(x)]
  }
  
  to_list_column <- function(.f = "na_values") {
    
    x <- dplyr::case_when ( 
      .f == "na_values" ~ sapply ( survey, labelled::na_values), 
      .f == "na_range"  ~ sapply ( survey, labelled::na_range), 
      .f == "valid_range"  ~ sapply ( survey, fn_valid_range),
      .f == "labels" ~ sapply ( survey, labelled::val_labels))
    
    x[sapply(x, is.null)] <- ""
   
    df <- purrr::map(x, list)
    names(df) <- rep(.f, length(df))
    df
  }
  
  range_df  <- tibble::tibble (
    var_name_orig = names(survey),
    labels = to_list_column(.f = "labels"),
    valid_range = to_list_column(.f = "valid_range"),
    na_values = to_list_column(.f = "na_values"),
    na_range = to_list_column (.f = "na_range"))
  
  range_df$n_labels <- vapply(1:nrow(range_df), function(x) length(unlist(range_df$labels[x])), numeric(1))
  range_df$n_cat_labels <- vapply(1:nrow(range_df), function(x) length(unlist(range_df$valid_range[x])), numeric(1))
  range_df$n_na_values <- vapply(1:nrow(range_df), function(x) length(unlist(range_df$na_values[x])), numeric(1))
  
  range_df$n_labels <- ifelse ( sapply ( range_df$labels, function(x) nchar(unlist(x))[1] ) == 0, 
                                   0,range_df$n_labels)
  range_df$n_cat_labels <- ifelse ( sapply ( range_df$valid_range, function(x) nchar(unlist(x))[1] ) == 0, 
                                   0,range_df$n_cat_labels)
  
  range_df$n_na_values <- ifelse ( sapply ( range_df$na_values, function(x) nchar(unlist(x))[1] ) == 0, 
           0,range_df$n_na_values)
  
 
  return_df <- metadata %>%
    dplyr::left_join ( range_df %>% 
                         dplyr::group_by ( var_name_orig ) %>%
                         tidyr::nest() , 
                       by = "var_name_orig") %>%
    tidyr::unnest ( cols = "data" )  %>%
    dplyr::ungroup() %>%
    dplyr::mutate ( n_na_values = as.numeric(n_na_values), 
             n_cat_labels = as.numeric(n_cat_labels), 
             n_labels = as.numeric(n_labels)) %>%
    as.data.frame()
}
