#' @title Read csv file
#'
#' @description Import a survey from a csv file.
#'
#' @param file A path to a file to import.
#' @param dataset_bibentry A bibliographic entry created with
#' \code{dataset::\link[dataset:dublincore]{dublincore}} or
#' \code{dataset::\link[dataset:datacite]{datacite}}.
#' @param id An identifier of the tibble, if omitted, defaults to the
#' file name without its extension.
#' @param doi An optional document object identifier.
#' @param ... Further optional parameters to pass on to
#' \code{utils::\link[utils:read.csv]{read.csv}}.
#' @importFrom tibble rowid_to_column
#' @return A tibble, data frame variant with survey attributes.
#' @importFrom fs path_ext_remove path_file is_file
#' @importFrom labelled var_label
#' @importFrom purrr safely
#' @importFrom utils read.csv
#' @importFrom fs path_ext_remove
#' @importFrom tibble as_tibble
#' @importFrom dplyr any_of select
#' @importFrom utils object.size
#' @importFrom labelled to_labelled `var_label<-`
#' @importFrom dataset dataset_df is.dataset_df dublincore datacite
#' @importFrom dataset defined
#' @family import functions
#' @examples
#' # Create a temporary CSV file:
#' path <- system.file("examples", "ZA7576.rds", 
#'                     package = "retroharmonize")
#' read_survey <- read_rds(path)
#' test_csv_file <- tempfile()
#' write.csv(x = read_survey, 
#'           file = test_csv_file, 
#'           row.names = FALSE)
#'          
#' # Read the CSV file:
#' re_read <- read_csv(
#'   file = test_csv_file,
#'   id = "ZA7576", 
#'   doi = "test_doi"
#' )
#' @export

read_csv <- function(file,
                     id = NULL,
                     doi = NULL,
                     dataset_bibentry = NULL,
                     ...) {
  # filename <- fs::path_file(file)

  filename <- file
  source_file_info <- valid_file_info(file)

  if (is.null(id)) {
    id <- fs::path_ext_remove(filename)
  }

  safely_readcsv <- purrr::safely(read.csv)

  tmp <- safely_readcsv(file = file)
  
  if (!is.null(tmp$error)) {
    warning(tmp$error, "\nReturning an empty survey.")
    return(
      survey(data.frame(), 
             id = "Could not read file",
             filename = filename, 
             doi = doi)
    )
  } else {
    tmp <- tmp$result
  }

  tmp <- tmp %>% dplyr::select(-any_of("X"))
  
  chr_vars <- vapply(1:ncol(tmp), 
                     function(x) inherits(tmp[, x], "character"), 
                     logical(1))
  
  chr_unique_n <- vapply(which(chr_vars), 
                         function(x) length(unique(tmp[x, ])), 
                         integer(1))
  
  to_fct_vars <- which(chr_vars)[which(chr_unique_n > 1)]


  for (i in to_fct_vars) {
    tmp[, i] <- dataset::defined(as.factor(tmp[, i]))
  }

  tmp_df <- dataset_df(tmp, 
                       identifier = doi, 
                       dataset_bibentry = dataset_bibentry)


  if (is.null(doi)) {
    if ("doi" %in% names(tmp)) {
      doi <- tmp$doi[1]
    } else {
      doi <- ""
    }
  }
  
  if (is.null(id)) {
    tmp_df$rowid <- paste0("survey_", tmp$rowid)
  } else { 
    tmp_df$rowid <- paste0(id, "_", gsub(id, '', tmp$rowid))
  }

  var_label(tmp_df$rowid) <- paste0("Unique identifier in ", id)

  return_survey <- survey_df(x = tmp, 
                             dataset_bibentry = dataset_bibentry, 
                             identifier = id, 
                             filename = filename)

  object_size <- as.numeric(object.size(as_tibble(tmp)))
  attr(return_survey, "object_size") <- object_size
  attr(return_survey, "source_file_size") <- source_file_info$size

  if (dataset::dataset_title(return_survey) == "Untitled Dataset") {
    dataset::dataset_title(return_survey, 
                           overwrite = TRUE) <- "Untitled Survey"
  }

  ## For backward compatibility
  attr(return_survey, "id") <- id
  attr(return_survey, "doi") <- doi


  return_survey
}
