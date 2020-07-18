#' Read survey from rds file 
#'
#' @param file A re-saved survey, imported with \code{haven::\link[haven:read_spss]{read_spss}}
#' @param id An identifier of the tibble, if omitted, defaults to the
#' file name.
#' @param doi An optional document object identifier.
#' @param filename An import file name.
#' @importFrom tibble rowid_to_column
#' @return A tibble, data frame variant with survey attributes.
#' @importFrom fs path_ext_remove path_file
#' @importFrom labelled var_label
#' @family import functions
#' @examples
#' path <-  system.file("examples", "ZA7576.rds", package = "retroharmonize")
#' read_survey <- read_rds(path)
#' attr(read_survey, "id")
#' attr(read_survey, "filename")
#' attr(read_survey, "doi") 
#' @export

read_rds <- function(file,
                      id = NULL, 
                      filename = NULL, 
                      doi = NULL) {
  
  if (! file.exists(file) ) stop ("The file does not exist.")
  filename <- fs::path_file(file)
  
  tmp <- readRDS (file = file) %>%
    tibble::rowid_to_column()
  
  if ( is.null(id) ) {
    id <- fs::path_ext_remove ( filename )
  }
  
  if ( is.null(doi)) {
    if ( "doi" %in% names(tmp) ) {
      doi <- tmp$doi[1]
    }
  }
  
  tmp$rowid <- paste0(id, "_", tmp$rowid)
  labelled::var_label ( 
    tmp$rowid ) <- paste0("Unique identifier in ", id)
  
  survey (tmp, id=id, filename, doi)
}