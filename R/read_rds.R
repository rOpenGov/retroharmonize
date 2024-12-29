#' @title Read rds file
#' 
#' @description Import a survey from an rds file.
#'
#' @param file A path to a file to import.
#' @param id An identifier of the tibble, if omitted, defaults to the
#' file name without its extension.
#' @param doi An optional document object identifier.
#' @param dataset_bibentry A bibliographic entry created with 
#' \code{dataset::\link[dataset:dublincore]{dublincore}} or 
#' \code{dataset::\link[dataset:datacite]{datacite}}.
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom fs path_ext_remove path_file is_file
#' @importFrom labelled var_label
#' @importFrom purrr safely
#' @importFrom utils object.size
#' @importFrom dataset dataset_df is.dataset_df
#' @return A tibble, data frame variant with survey attributes.
#' @family import functions
#' @examples
#' path <-  system.file("examples", "ZA7576.rds", package = "retroharmonize")
#' read_survey <- read_rds(path)
#' attr(read_survey, "id")
#' attr(read_survey, "filename")
#' attr(read_survey, "doi") 
#' @export

read_rds <- function(file,
                     dataset_bibentry = NULL,
                     id = NULL, 
                     doi = NULL) {
  
  source_file_info <- valid_file_info(file)
  filename <- fs::path_file(file)
  
  if ( is.null(id) ) {
    id <- fs::path_ext_remove ( filename )
  }
  
  safely_readRDS <- purrr::safely (readRDS)
  
  tmp <- safely_readRDS (file = file)  
  
  if ( ! is.null(tmp$error) ) {
    warning ( tmp$error, "\nReturning an empty survey." )
    return(
      survey ( data.frame(), id="Could not read file", filename=filename, doi=doi)
    )
  } else {
    tmp  <- tmp$result
  }
  
  source_file_info <- valid_file_info(file)
  
  #if ( ! "rowid" %in% names(tmp) ) {
  #  tmp <- tibble::rowid_to_column(tmp)
  #}
  
  tmp_df <- dataset_df(tmp, identifier = doi, dataset_bibentry = dataset_bibentry )
  
  if ( is.null(doi)) {
    if ( "doi" %in% names(tmp_df) ) {
      doi <- tmp_df$doi[1]
    } else {
      doi <- ""
    }
  }
  
  tmp_df$rowid <- paste0(id, "_", gsub(id, "", tmp_df$rowid))
  labelled::var_label ( 
    tmp_df$rowid ) <- paste0("Unique identifier in ", id)
  
  return_survey <- survey (tmp_df, id=id, filename=filename, doi=doi)
  
  object_size <- as.numeric(object.size(as_tibble(tmp_df)))
  attr(return_survey, "object_size") <- object_size
  attr(return_survey, "source_file_size") <- source_file_info$size
  
  return_survey
}