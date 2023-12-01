#' @title Read survey file(s)
#'
#' @description Import surveys into a list or several \code{.rds} files. 
#' 
#' @details Use \code{read_survey} for a single survey and \code{read_surveys} for several surveys in
#' in a loop. The function handle exceptions with wrong file names and not readable 
#' files. If a file cannot be read, a message is printed, and empty survey is added to the 
#' the list in the place of this file. 
#'
#' @param survey_paths A vector of (full) file paths that contain the surveys to import.
#' @param .f A function to import the surveys with.
#' Defaults to \code{'NULL'}, in this case files with an extension of \code{'.sav'} and \code{'.por'}
#' will call case \code{\link{read_spss}}, files with an extension of \code{'.dta'} will 
#' call \code{\link{read_dta}}, \code{rds} will call \code{\link{read_rds}} and
#' \code{'.csv'} \code{\link{read_csv}}.
#' @param export_path Defaults to \code{NULL}, in this case the read surveys are imported into a single
#' list of surveys in memory. If \code{export_path} is a valid directory, it will instead save each 
#' survey an R object with \code{base::\link[base:readRDS]{saveRDS}}.
#' @return A list of the surveys or a vector of the saved file names.  
#' Each element of the list is a data
#' frame-like \code{\link{survey}} type object where some metadata, 
#' such as the original file name, doi identifier if present, and other
#' information is recorded for a reproducible workflow.
#' @importFrom purrr safely
#' @importFrom fs path_file
#' @importFrom assertthat assert_that
#' @examples
#' file1 <- system.file(
#'     "examples", "ZA7576.rds", package = "retroharmonize")
#' file2 <- system.file(
#'     "examples", "ZA5913.rds", package = "retroharmonize")
#'
#' read_surveys (c(file1,file2), .f = 'read_rds' )
#' @export
#' @family import functions
#' @seealso survey

read_surveys <- function ( survey_paths,
                           .f = NULL,
                           export_path = NULL ) {
  
  import_file_vector <- survey_paths
  existing_files <- which(file.exists(import_file_vector))
  not_existing_files <- which(! file.exists(import_file_vector))
  
  if ( length(existing_files)==0) {
    stop ("None of the files on read_surveys(survey_paths=...) exist.")
  }
  
  if (length(not_existing_files)>0) {
    missing_files <- paste(import_file_vector[not_existing_files], collapse = ";\n")
    warning("Some files on 'survey_pahts' do not exist:\n", missing_files)
  }
  
  import_file_vector <- import_file_vector[existing_files]
  
  return_survey_list <- lapply ( import_file_vector, 
                                 function(x) read_survey(x, .f, export_path)
                                 )
  return_survey_list
}

#' @rdname read_surveys
#' @importFrom fs file_exists dir_exists path_ext_remove
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom purrr safely

read_survey <- function(file_path, .f = NULL, export_path = NULL) {

  assert_that(fs::file_exists(file_path), 
              msg = glue::glue("The file {file_path} does not exist."))
  
  if (is.null(.f)) .f <- find_import_function(file_path) ## See definition below
  
  if ( .f == "read_rds") {
    res <- safely(read_rds)(file_path)
  } else if ( .f == 'read_spss') {
    res <- safely(read_spss)(file_path)
  } else if ( .f == 'read_dta') {
    res <- safely(read_dta)(file_path)
  } else if ( .f == 'read_csv') {
    res <- safely(read.csv)(file_path)
  } 
  
  if (is.null(res$error)) {
    # No problem reading and should be saved --------------------------------------
    if (!is.null(export_path)) {
      if ( fs::dir_exists(export_path) ) {
        # Returned survey ----------------------------------------------------------------
        imported_survey <- res$result
        source_file_name <- attr(imported_survey, "filename")
        
        # Saving location exists, return file name after saving --------------------
        new_file_name <- paste0(fs::path_ext_remove(source_file_name), ".rds")
        saveRDS(res$result,
                file =  file.path(export_path, new_file_name), 
                version = 2)
        return(new_file_name)
      } else  {
        # Exception: cannot be exported, returning to  memory -------------------------------
        warning("Cannot save to ", export_path, ", returning to memory instead.")
        return(res$result) }
    }
    return(res$result)
    }
    
  if ( !is.null(res$error)) {
      # There was a problem reading -------------------------------------------------
      # Even though the file exists (checked in the beginning of the function) ------
      message(res$error)
      message ("This is an error in read_survey(", file_path, ", ", .f, ")")
      message("Returning NULL for this file.")
      return(NULL)
    }
  
}


#' @title Find import function by file extension
#' @description This is an internal utility to select the appropriate importing function.
#' @return The name of the function that should read \code{file_path} based on the file 
#' extension.
#' @importFrom fs path_ext path_ext_remove path_file
#' @importFrom glue glue
#' @inheritParams read_surveys
#' @keywords internal
find_import_function <- function(file_path) {
  
  survey_file_ext <- fs::path_ext(file_path)
  
  if ( survey_file_ext %in% c("sav", "por") ) {
    'read_spss'
  } else if (survey_file_ext == "rds") {
    'read_rds'
  } else if ( survey_file_ext == "dta") {
    'read_dta'
  } else if ( survey_file_ext == "csv") { 
    'read_csv'
  } else {
    stop(glue("No adequate importing function was found for {file_path}."))
  }
}
