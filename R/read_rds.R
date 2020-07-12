#' Read survey.rds 
#'
#' @param file A re-saved survey, imported with \code{haven::\link[haven:read_spss]{read_spss}}
#' @param id An identifier of the tibble, if omitted, defaults to the
#' file name.
#' @param doi An optional document object identifier.
#' @param filename An import file name.
#' @importFrom tibble rowid_to_column
#' @importFrom fs path_file
#' @return A tibble, data frame variant with survey attributes
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
  
  tmp <- readRDS (file = file) %>%
    tibble::rowid_to_column()
  
  filename <- fs::path_file(path)
  if ( is.null(id)) id <- fs::path_file(path)
  
  tmp$rowid <- paste0(id, "_", tmp$rowid)
  
  survey (tmp, id =id, filename, doi)
}
