#' @title Subset surveys
#' 
#' @description This is a wrapper function for various procedures to reduce the size of surveys
#' by removing variables that are not harmonized.
#' 
#' @details This function allows several workflows. 
#' Subsetting can be based on a vector of variable names
#' given by \code{survey_path}, or on the basis of a \code{crosstable}. 
#' The \code{\link{subset_save_surveys}} can be called directly. 
#' 
#' \code{subset_surveys} will also harmonize the variable names if the \code{var_name_target} is 
#' optionally defined in the \code{crosswalk_table} input.  
#' \code{harmonize_survey_variables} is a wrapper and will require that the new (target) variable names are
#' present in a valid \code{crosstable}. 
#' 
#' @param crosswalk_table A crosswalk table created by \code{\link{crosswalk_table_create}} or a manually created 
#' crosstable including at least
#' \code{filename}, \code{var_name_orig}, \code{var_name_target} and optionally 
#'  \code{var_label_orig} and \code{var_label_target}. This parameter is optional and 
#'  defaults to \code{NULL}.
#' @param survey_list A list of surveys imported with \code{\link{read_surveys}}. If set to 
#' \code{NULL}, the \code{survey_path} should give full path to the surveys.
#' @param survey_paths A vector of full file paths to the surveys to subset. 
#' @param rowid The unique row (observation) identifier in the files. Defaults to 
#' \code{"rowid"}, which is the default of the importing functions in this package.
#' @param subset_name An identifier for the survey subset.
#' @param subset_vars The names of the variables that should be kept from all surveys in the list that contains the
#' wave of surveys. Defaults to \code{NULL} in which case it returns all variables without subsetting.
#' @importFrom dplyr select any_of
#' @family subsetting function
#' @return A list of surveys or save individual rds files on the \code{export_path}.
#' @examples
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)
#' subset_surveys(survey_list = example_surveys, 
#'               subset_vars = c("rowid", "isocntry", "qa10_1", "qa14_1"), 
#'               subset_name = "subset_example")
#' @export

subset_surveys <- function ( survey_list, 
                             survey_paths = NULL,
                             rowid = "rowid",
                             subset_name = "subset",
                             subset_vars = NULL, 
                             crosswalk_table = NULL, 
                             import_path = NULL, 
                             export_path = NULL) {
  
  if ( !is.null(survey_list)) {
    validate_survey_list(survey_list)
    subset_from_files <- FALSE
    files_to_subset <- NULL
    
    if ( !"list" %in% class(survey_list) & is.survey(survey_list) ) {
      survey_list <- list ( i = survey_list )
      names(survey_list)[1] <- attr(survey_list[[1]], "id")
    }
  }    else {
    subset_from_files <- TRUE
  }
  if ( !is.null(survey_paths) )   validate_survey_files(survey_paths)
  if ( !is.null(crosswalk_table)) { 
    is.crosswalk_table(crosswalk_table) 
    if (!is.null(survey_paths)) {
      files_to_subset <- survey_paths [fs::path_file(survey_paths) %in% fs::path_file(crosswalk_table$filename)]
    } else if ( !is.null(import_path)) {
      files_to_subset <- dir(import_path) [dir(import_path) %in% fs::path_file(crosswalk_table$filename)]
    } else {
      files_to_subset <- unique(crosswalk_table$filename)
    }
    
  }
  if ( !is.null(import_path)) {
    assert_that(fs::dir_exists(import_path) == TRUE, 
                msg = " in subset_surveys: the 'import_path' is not a valid path to a directory.")
    
    files_to_subset <- dir(import_path)[dir(import_path) %in% fs::path_file(crosswalk_table$filename)]
    files_to_subset <- file.path(import_path, files_to_subset)
  }
  if ( !is.null(export_path)) {
    assert_that(fs::dir_exists(export_path) == TRUE, 
                msg = " in subset_surveys: the 'export_path' is not a valid path to a directory.")
    
  }
  
  if ( subset_from_files ) {
    ## Subsetting from files 
    if (length(files_to_subset) ==0) {
      message ("subset_surveys(): No files to subset.")
      return(NULL)}

    ### Subsetting with crosswalk table ----------------------
    if ( !is.null(crosswalk_table)) {
      get_survey <- function(x) {
        this_path <- files_to_subset[x]
        
        this_id <- crosswalk_table %>% filter (
          .data$filename == fs::path_file(this_path)
        ) %>% distinct(.data$id) %>% unlist() %>% as.character()
        
        subset_vars <- crosswalk_table %>%
          filter ( .data$id == this_id ) %>%
          select ( .data$var_name_orig ) %>%
          unlist() %>%
          as.character() %>% unique()
        
        subset_survey_files(
          file_path = this_path, 
          subset_vars = subset_vars, 
          id = this_id, 
          export_path = export_path
        )
      }
      
      if (!is.null(export_path)) {
        lapply (seq_along(files_to_subset), function(x) get_survey(x))
        return(NULL)
      }
      return_list  <- lapply (seq_along(files_to_subset), function(x) get_survey(x))
    } else {
      ## Subsetting without crosswalk table -----------------
      
      get_survey_no_ctable <- function(x) {
        this_path <- files_to_subset[x]
       
        subset_survey_files(
          file_path = this_path, 
          subset_vars = subset_vars, 
          id = this_id, 
          export_path = export_path
        )
        }
      
      if (!is.null(export_path)) {
        lapply (seq_along(files_to_subset), function(x) get_survey_no_ctable(x))
        return(NULL)
      }
      return_list <- lapply (seq_along(files_to_subset), function(x) get_survey_no_ctable(x))
    }
     
    
  } else {
    ### Subsetting from memory, with cross_table ------------------------------
    
    if (!is.null(crosswalk_table)) {
      
      available_surveys <- vapply(survey_list, function(x) attr(x, "id"), character(1))
      surveys_in_ctable <- unique(crosswalk_table$id)[unique(crosswalk_table$id) %in% available_surveys]
      
      s <- which(surveys_in_ctable == available_surveys)
      
      get_survey_memory <- function(x) {
        subset_vars <- crosswalk_table %>%
          select ( .data$var_name_orig ) %>%
          unlist() %>%
          as.character() %>% unique()
        
        subset_survey_memory(
          this_survey = survey_list[[x]],
          subset_vars = subset_vars, 
          subset_name = subset_name, 
          export_path = export_path
        )
      }
      if (!is.null(export_path)) {
        lapply (seq_along(s), function(x) get_survey_memory(x))
        return(NULL)
      } else {
        return_list <- lapply (seq_along(s), function(x) get_survey_memory(x))
      }
      
      
    } else {
      ### Subsetting from memory, without cross_table ------------------------------
      get_survey_no_ctable_memory <- function(x) {
        subset_survey_memory(
          this_survey = survey_list[[x]],
          subset_vars = subset_vars, 
          subset_name = subset_name, 
          export_path = export_path
        )
      }
      
      if (!is.null(export_path)) {
        lapply (seq_along(survey_list), function(x) get_survey_no_ctable_memory(x))
        return(NULL)
      } else {
        return_list <- lapply (seq_along(survey_list), function(x) get_survey_no_ctable_memory(x))
      }
    }
  }
  return_list
}
  

#' @rdname subset_surveys
#' @param waves A list of surveys imported with \code{\link{read_surveys}}.
#' @export
subset_waves <- function( waves, subset_vars = NULL) {
  .Deprecated(new = "subset_surveys", msg = "subset_waves is deprecated, use subset_surveys instead.")
  subset_surveys ( survey_list = waves, subset_vars = subset_subset_vars )  
}


#' @rdname subset_surveys
#' @export
subset_save_surveys  <- function ( crosswalk_table, 
                                   subset_name = "subset",
                                   survey_list = NULL,
                                   survey_paths = NULL,
                                   import_path = NULL, 
                                   export_path = NULL) {
  .Deprecated(new = "subset_surveys", 
              msg = "subset_save_surveys is deprecated, use subset_surveys instead.")
  subset_surveys ( 
    crosswalk_table = crosswalk_table, 
    subset_name = subset_name, 
    survey_list = survey_list, 
    subset_vars = subset_vars, 
    import_path = import_path, 
    export_path = export_path)  
}

#' @title Subset surveys from files
#' @inheritParams subset_surveys
#' @param file_path A single survey files. 
#' @keywords internal
subset_survey_files <- function( file_path, 
                                 subset_vars, 
                                 subset_name = 'subset',
                                 id = NULL, 
                                 export_path = NULL ) {
  
  subset_vars <- unique(c("rowid",subset_vars))
  
  survey_file_ext <- fs::path_ext(file_path)
  if ( is.null(id)) id <- fs::path_ext_remove(fs::path_file(file_path))
  
  if ( survey_file_ext %in% c("sav", "por")) {
    this_survey <- read_spss(file_path, id = id)
  } else if (survey_file_ext == "rds") {
    this_survey <- read_rds(file = file_path, id = id)
  } else if ( survey_file_ext == "dta") {
    this_survey <- read_dta(file = file_path, id = id )
  } else if ( survey_file_ext == "csv") { 
    this_survey <- read_csv(file = file_path, id = id )
  } else {
    return(NULL)
  }
  subset_survey_memory (this_survey, 
                        subset_vars = subset_vars,
                        subst_name = subset_name, 
                        export_path = export_path)

}

#' @title Subset surveys in memory
#' @inheritParams subset_surveys
#' @importFrom tibble as_tibble
#' @keywords internal
subset_survey_memory <- function(this_survey, 
                                 subset_vars, 
                                 subset_name = 'subset', 
                                 export_path = NULL ) {
  
  subset_vars <- unique(c("rowid", subset_vars))
  
  subset_survey <- this_survey %>%
    select (any_of(subset_vars))
  
  attr(subset_survey, "subset_size") <- as.numeric(object.size(as_tibble(subset_survey)))
  
  this_file_name <- paste0(
    fs::path_ext_remove(attr(subset_survey, "filename") ), 
    "_", subset_name, ".",
    fs::path_ext(attr(subset_survey, "filename"))) 
  
  attr(subset_survey, "filename") <- this_file_name 
  
  
  if(!is.null(export_path)) { 
    save_file_name <- paste0(fs::path_ext_remove(fs::path_file(this_file_name)), ".rds")
    message("Saving ", paste0(fs::path_ext_remove(fs::path_file(this_file_name)), ".rds"))
    saveRDS(object = subset_survey_memory (this_survey, subset_vars), 
            file = file.path(export_path, save_file_name), 
            version = 2)
  } else {
    subset_survey
  }
}


#' @rdname subset_surveys
#' @importfrom dplyr add_count
#' @examples 
#' test_survey <- read_rds (
#'   file = system.file("examples", "ZA7576.rds",
#'                      package = "retroharmonize")
#' )
#' 
#'   test_metadata <- metadata_create ( test_survey )
#'   test_metadata <- test_metadata[c(1,7,18),]
#'   ctable_2 <- crosswalk_table_create(test_metadata)
#'   ctable_2$var_name_target  <- ifelse(ctable$var_name_orig == "qa14_3", 
#'                                      "trust_ecb", 
#'                                       ctable$var_name_orig)
#' 
#' subset_save_surveys  ( crosswalk_table = ctable_2, 
#'                        subset_name = "tested",
#'                        survey_list =  test_survey,
#'                        import_path = NULL
#' )
#' @export