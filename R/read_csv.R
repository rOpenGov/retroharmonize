#' @title Read csv file
#' 
#' @description Import a survey from a csv file.
#' 
#' @inheritParams read.csv
#' @param file A path to a file to import.
#' @param id An identifier of the tibble, if omitted, defaults to the
#' file name without its extension.
#' @param doi An optional document object identifier.
#' @importFrom tibble rowid_to_column
#' @return A tibble, data frame variant with survey attributes.
#' @importFrom fs path_ext_remove path_file is_file
#' @importFrom labelled var_label
#' @importFrom purrr safely
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @importFrom purrr safely
#' @family import functions
#' @examples
#' path <-  system.file("examples", "ZA7576.rds", package = "retroharmonize")
#' read_survey <- read_rds(path)
#' attr(read_survey, "id")
#' attr(read_survey, "filename")
#' attr(read_survey, "doi") 
#' @export

read_csv <- function(file,
                     id = NULL, 
                     filename = NULL, 
                     doi = NULL, 
                     header = FALSE, 
                     sep = "", quote = "\"'",
                     dec = ".",
                     numerals = c("allow.loss", "warn.loss", "no.loss"),
                     na.strings = "NA", 
                     skip = 0, check.names = TRUE, 
                     strip.white = FALSE, 
                     blank.lines.skip = TRUE,
                     stringsAsFactors = FALSE,
                     fileEncoding = "", encoding = "unknown") {
  
  filename <- fs::path_file(file)
  
  if ( is.null(id) ) {
    id <- fs::path_ext_remove ( filename )
  }
  
  safely_readcsv <- purrr::safely (read.csv)
  
  
  tmp <- safely_readcsv(file = file,
                        na.strings = na.strings, 
                        dec = dec, 
                        skip = skip, 
                        strip.white = strip.white,
                        blank.lines.skip = blank.lines.skip,
                        numerals = numerals,
                        stringsAsFactors = stringsAsFactors, 
                        fileEncoding = fileEncoding, 
                        encoding = encoding)  

  if ( ! is.null(tmp$error) ) {
    warning ( tmp$error, "\nReturning an empty survey." )
    return(
      survey ( data.frame(), id="Could not read file", filename=filename, doi=doi)
    )
  } else {
    tmp  <- tmp$result
  }
  
  source_file_info <- valid_file_info(file)
  
  if ( ! "rowid" %in% names(tmp) ) {
    tmp <- tibble::rowid_to_column(tmp)
  }
  
  if ( is.null(doi)) {
    if ( "doi" %in% names(tmp) ) {
      doi <- tmp$doi[1]
    } else {
      doi <- ""
    }
  }
  
  tmp$rowid <- paste0(id, "_", gsub(id, "", tmp$rowid))
  labelled::var_label ( 
    tmp$rowid ) <- paste0("Unique identifier in ", id)
  
  return_survey <- survey (tmp, id=id, filename=filename, doi=doi)
  
  object_size <- as.numeric(object.size(as_tibble(tmp)))
  attr(return_survey, "object_size") <- object_size
  attr(return_survey, "source_file_size") <- source_file_info$size
  
  return_survey
}
