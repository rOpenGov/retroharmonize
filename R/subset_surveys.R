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
#' @param crosstable A crosstable created by \code{\link{crosstable_create}} or a manually created 
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
#' @family harmonization functions
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
  
  validate_survey_list(survey_list)
  
  if ( !is.null(subset_vars) ) {
    subset_vars <- unique(c(rowid, subset_vars))
    
    subset_survey <- function(this_survey) {
      
      tmp  <- this_survey %>% 
        select ( any_of ( subset_vars ) )
      
      this_file_name <- attr(tmp, "filename")
      new_file_name <- paste0(fs::path_ext_remove(this_file_name), "_", subset_name, ".", fs::path_ext(this_file_name ))
      attr(tmp, "filename") <- new_file_name
      
      tmp
    }
    
    lapply ( survey_list, subset_survey )
    
  } else  if ( !is.null(crosswalk_table) ) {
    
    is.crosswalk_table(crosswalk_table)
    
    subset_save_surveys(crosswalk_table = crosswalk_table, 
                        survey_list = survey_list, 
                        survey_paths = survey_paths, 
                        import_path = import_path,
                        export_path = export_path )
    
  } else {
    stop( ": in subset_surveys() - neither 'crosswalk_table' nor 'subset_vars' are given. ")
  }
}



#' @title Subset and save surveys
#'   
#' @inheritParams subset_surveys
#' @rdname subset_surveys
#' @param subset_name An identifier for the survey subset.
#' @param import_path The path to the survey files. Defaults to \code{NULL}.
#' @param export_path The path where the subsets should be saved. Defaults to \code{NULL} when
#' the surveys are not saved but returned as a list.
#' @importFrom dplyr distinct mutate  all_of distinct select
#' @importFrom fs file_exists path_ext dir_exists path_file path_ext_remove
#' @importFrom assertthat assert_that
#' @importFrom rlang set_names .data
#' @importFrom utils object.size
#' @return The function does not return a value. It saves the subsetted
#' surveys into .rds files.
#' @examples 
#' test_survey <- read_rds (
#'   file = system.file("examples", "ZA7576.rds",
#'                      package = "retroharmonize")
#' )
#' 
#'   test_metadata <- metadata_create ( test_survey )
#'   test_metadata <- test_metadata[c(1,7,18),]
#'   ctable <- crosswalk_table_create(test_metadata)
#' 
#' subset_save_surveys  ( crosswalk_table = ctable, 
#'                        subset_name = "tested",
#'                        survey_list =  test_survey,
#'                        import_path = NULL
#' )
#' @export

subset_save_surveys  <- function ( crosswalk_table, 
                                   subset_name = "subset",
                                   survey_list = NULL,
                                   survey_paths = NULL,
                                   import_path = NULL, 
                                   export_path = NULL) {
  
  
  if (is.null(survey_list) & is.null(import_path)) {
    stop(" in subset_save_surveys(...survey_list, import_path, ...): both the 'survey_list' and the 'import_path' is missing." )
  }
  
  if ( !is.null(survey_list)) {
    ## Importing from a list in memory
    validate_survey_list(survey_list = survey_list)
    subset_from_files <- FALSE
    if ( ! "list" %in% class(survey_list) ) {
      survey_id <- attr(survey_list, "id") 
      survey_list <- list( i = survey_list )
      names(survey_list)[1] <- survey_id    } 
    
    imported_survey_files <- vapply ( survey_list, function(x) attr(x, "filename"), character(1) )
    } else {
    # Importing from a files
    assert_that(fs::dir_exists(import_path) == TRUE, 
                msg = " in subset_save_surveys: the 'import_path' is not a valid path to a directory.")
    subset_from_files <- TRUE
  }
  
  assert_that(
    all(c("filename", "id", "var_name_orig") %in% names(crosswalk_table) ), 
    msg = "in subset_save_surveys(croswalk_table,....): the crosswalk table must have 'filename', 'id', 'var_name_orig' columns."
  )
  
  ## Subsetting the crosswalk table for this particular task ---------------------------------
  selection <- crosswalk_table %>%
    distinct ( 
      across (any_of(c("filename", "id", 
                       "var_name_orig", "var_name_target", 
                       "var_label_orig", "var_label_target")
      )
      )
    )
  
  if ( ! "var_name_target" %in% names(selection) ) {
    selection <- selection %>% 
      mutate ( var_name_target = .data$var_name_orig )
  }
  
  ## Finding the location of the surveys that need to be subsetted ---------------------------
  if ( is.null(survey_paths) ) {
    survey_files <- selection %>%
      distinct ( across (all_of(c("filename", "id"))) )
    subset_survey_files <- survey_files$filename
  } 
  
  if ( !is.null(import_path) ) {
    validate_survey_files(vapply ( subset_survey_files, 
                                   function(x) file.path(import_path, x), 
                                   character(1))  )
  } else { 
    ## if survey_paths are given as full path
    validate_survey_files(survey_paths)
  }
  
  ## A sequential subsetting of the surveys -----------------------------------------------------
  
  for (i in seq_along(subset_survey_files) ) {
    
    if ( subset_from_files ) {
      ## The subsetting requires importing the surveys from files ------------------------------
      
      if ( is.null(import_path) ) {
        this_file <- survey_paths[i]
        this_file_name <-  fs::path_file(survey_paths)
        
      } else {
        this_file <- file.path(import_path, subset_survey_files[i])
        this_file_name <- subset_survey_files[i]
      }
      
      if ( ! fs::file_exists(this_file) ) {
        warning( this_file, " does not exist.")
        next
      }
      
      this_ext <- fs::path_ext(this_file)
      
      if ( this_ext %in% c("sav", "por")) {
        this_survey <- read_spss(this_file, id = survey_files$id[i])
      } else if (this_ext == "rds") {
        this_survey <- read_rds(file = this_file, id = survey_files$id[i])
      } else if ( this_ext == "dta") {
        this_survey <- read_dta(file = this_file, id = survey_files$id[i])
      } else {
        next
      }
      
    } else {
      ## Subseting takes place from  a list in memory -------------------------------------------
      this_file_name <-  fs::path_file(subset_survey_files[i])
      this_survey <- survey_list[[which( imported_survey_files == this_file_name)]]
    }
    
    ## If no new names are given, this will be identical to the old names
    survey_vars <- selection %>%
      select ( any_of(c("filename", "var_name_orig", "var_name_target", 
                        "var_label_orig", "var_label_target"))) %>% 
      filter ( .data$filename == this_file_name ) 
    
    
    new_names <- as.character(survey_vars$var_name_target)
    
    renaming_table <- survey_vars %>%
      distinct ( .data$var_name_orig, .data$var_name_target )
    
    
    ## The actual subsetting --------------------------------------------------------------------
    subsetted_survey <- this_survey %>%
      select ( all_of (renaming_table$var_name_orig) ) %>%
      rlang::set_names( nm = renaming_table$var_name_target )
    
    ## Returning either a vector of the files created or a list ---------------------------------
    if ( !is.null(export_path) ) {
      # if the export_path is given then the surveys will be saved there.
      
      assert_that(fs::dir_exists(export_path) == TRUE, 
                  msg = " in subset_save_surveys: the 'export_path' is not a valid path to a directory.")
      
      save_file_name <- paste0(fs::path_ext_remove(this_file_name), "_", 
                               subset_name, ".rds")
      
      message ( "Saving ", save_file_name )
      
      saveRDS(subsetted_survey, file = file.path(
        export_path, save_file_name ), 
        version = 2)
      
      if ( i == 1 ) {
        return_file_vector <- file.path(export_path, save_file_name )
      } else {
        return_file_vector <- c(
          return_file_vector, 
          file.path(export_path, save_file_name)
        )
      }
    } else {
      ## If export_path is given, the subsetted surveys are returned as a list
      if (i == 1 ) {
        return_list <- list ( i = subsetted_survey)
        names(return_list)[i] <- attr(this_survey, "id")
        
      } else {
        return_list <- c( return_list, list ( i = subsetted_survey))
        names(return_list)[i] <- attr(this_survey, "id")
      }
    }
  }
  
  if ( is.null(export_path) ) {
    return_list
  } else {
    return_file_vector
  }
}

#' @rdname subset_surveys
#' @inheritParams subset_surveys
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

harmonize_survey_variables <- function( crosswalk_table, 
                                 subset_name = "subset",
                                 survey_list = NULL,
                                 survey_paths = NULL,
                                 import_path = NULL, 
                                 export_path = NULL ) {
  
  ## This is a wrapper for subset_save_survey with strict validation of new variable names.
  
  is.crosswalk_table(crosswalk_table)
  
  ## selection: relevant metadata for this particular survey
  selection <- crosswalk_table %>% 
    filter ( .data$id == survey_id ) %>%
    distinct_all()
  
  ## metadata for the harmonization of variable names 
  crosswalk_var_names <- selection %>%
    select  ( all_of(c("var_name_orig", "var_name_target")) ) %>%
    distinct_all() %>%
    add_count (.data$var_name_target)
  
  multiple_target_names <- crosswalk_var_names$var_name_target[which(crosswalk_var_names$n>1)]
  multiple_target_names <- paste(multiple_target_names, collapse = ", ")
  
  assertthat::assert_that(
    # There should be no unambigous names, duplicates are not allowed.
    nchar(multiple_target_names)==0,
    msg = glue("The following names are duplicated in {survey_id}: {multiple_target_names}")
  )
  
  subset_save_surveys(crosswalk_table = crosswalk_table, 
                      subset_name = subset_name, 
                      survey_list = survey_list,
                      import_path  = import_path,
                      export_path = export_path )
  
}

#' @rdname subset_surveys
#' @param waves A list of surveys imported with \code{\link{read_surveys}}.
#' @export
subset_waves <- function( waves, subset_vars = NULL) {
  .Deprecated(new = "subset_surveys", msg = "subset_waves is deprecated, use subset_surveys instead.")
  subset_surveys ( survey_list = waves, subset_vars = subset_subset_vars )  
}