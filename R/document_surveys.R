#' @title Document survey lists
#' 
#' @description Document the key attributes surveys in a survey list.
#' 
#' @details The function has two alternative input parameters. If \code{survey_list} is the 
#' input, it returns  the name of the original source data file, the number of rows and 
#' columns, and the size of the object as stored in memory. In case \code{survey_paths}
#' contains the source data files, it will sequentially read those files, and add the file
#' size, the last access and the last modified time attributes. 
#'
#' @param survey_list A list of \code{\link{survey}} objects.
#' @param survey_paths A vector of full file paths to the surveys to subset, defaults to 
#' \code{NULL}.
#' @param .f A function to import the surveys with.
#' Defaults to \code{'read_rds'}. For SPSS files,
#' \code{read_spss} is recommended, which is a
#' well-parameterized version of \code{\link[haven]{read_spss}} that
#' saves some metadata, too. For STATA files use \code{read_dta}.
#' @return Returns a data frame with the key attributes of the surveys 
#' in a survey list: the name of the data file, the number of rows and 
#' columns, and the size of the object as stored in memory.
#' @importFrom tibble tibble
#' @importFrom here here
#' @family documentation functions
#' @examples
#' examples_dir <- system.file( "examples", package = "retroharmonize")
#'                         
#' my_rds_files <- dir( examples_dir)[grepl(".rds", 
#'                                    dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))
#'  
#' documented <- document_surveys(example_surveys)
#' 
#' attr(documented_surveys, "original_list")
#' documented_surveys
#' 
#' document_surveys(survey_paths = file.path(examples_dir, my_rds_files))
#' 
#' @export

document_surveys <- function(survey_list = NULL, 
                             survey_paths = NULL, 
                             .f = NULL ) {
  
  if ( !is.null(survey_list) ) {
    validate_survey_list(survey_list)
  } else if ( !is.null(survey_paths) ) {
    validate_survey_files(survey_paths)
  } else {
    stop(" in document_surveys() - both 'survey_list' and 'survey_path' is NULL.")
  }
  
  if ( !is.null(survey_list)) {
    n_survey <- length(survey_list)
    
    return_df <- tibble(
      id  = vapply ( survey_list, function(x) attr(x, "id"), character(1)), 
      filename =  vapply ( survey_list, function(x) attr(x, "filename"), character(1)), 
      ncol = vapply (survey_list, ncol, integer(1)), 
      nrow =  vapply ( survey_list, nrow, integer(1)),
      object_size =  vapply ( survey_list, object.size, double(1))
    )
    
    attr(return_df, "original_list")  <- deparse(substitute(survey_list))
    
    return_df 
  } else {
    
    survey_files <- fs::path_file(survey_paths)
    
    tmp <- tibble(
      id = fs::path_ext_remove(survey_files),
      filename = survey_files, 
      ncol = NA_real_, 
      nrow = NA_real_, 
      object_size = NA_real_,
      file_size = NA_real_, 
      accessed = NA_character_, 
      last_modified = NA_character_
    )
    
    for ( i in seq_along(survey_files)) {
      file_info <- file.info(survey_paths[i]) 
      tmp$file_size[i] <- file_info$size
      tmp$accessed[i] <- as.character(file_info$atime)
      tmp$last_modified[i] <- as.character(file_info$mtime) 
      }
    
    for ( i in seq_along(survey_files)) {
      message ( length(i), "/", i, " ", survey_files[i])
      tmp_survey <- read_survey( file_path = survey_paths[i] )
      if (!is.null(tmp_survey)) {
        tmp$object_size[i] <- object.size(tmp_survey) 
        tmp$ncol[i] <- ncol(tmp_survey)
        tmp$nrow[i] <- nrow(tmp_survey)
      } else {
        next
      }
    } 
     
     return_df <- tmp
    }
  
  return_df  
}

#' @rdname document_surveys
#' @details The earlier form \code{document_waves} is deprecated.  
#' Currently called \code{\link{document_surveys}}.
#' @param waves  A list of \code{\link{survey}} objects.
#' @export

document_waves <- function(waves) {
  .Deprecated("document_surveys", 
              msg = "document_waves() is deprecated. Use document_surveys() instead")
  merge_surveys( survey_list = waves)
}
