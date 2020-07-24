#' Collect labels from metadata file
#' 
#' Use \code{collect_val_labels} for the valid range and 
#' \code{collect_na_labels} for the user-defined missing value
#' range.
#' 
#' @param metadata A metadata data frame created by 
#' \code\link{metadata_create}
#' @return The unique valid labels or the user-defined missing 
#' labels found in all the files analysed in \code{metadata}.
#' @family harmonization functions
#' @examples 
#' test_survey <- retroharmonize::read_rds (
#'    file = system.file("examples", "ZA7576.rds",
#'                   package = "retroharmonize"), 
#'    id = "test"
#' )
#' example_metadata <- metadata_create (test_survey)
#'
#'collect_val_labels (metadata = example_metadata )
#'collect_na_labels ( metadata = example_metadata )
#' @export
collect_val_labels <- function(metadata) {
  
  valid_labels <- lapply (metadata$valid_labels, unlist) %>%
    sapply(., names )
  unique(as.character(unlist(valid_labels)))
  
}

#' @rdname collect_val_labels
#' @export
collect_na_labels <- function(metadata) {
  
  na_labels <- lapply (metadata$na_labels, unlist) %>%
    sapply(., names )
  unique(as.character(unlist(na_labels)))
  
}
usethis::use_test()
