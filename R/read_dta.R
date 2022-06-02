#' @title Read Stata DTA files (`.dta`) files
#' 
#' @description This is a wrapper around \code{haven::\link[haven:read_dta]{read_dta}} 
#' with some exception handling.
#'
#' @details `read_dta()` reads both `.dta`  files.
#'
#' The funcion is not yet tested.
#'
#' @param file A STATA file.
#' @param .name_repair Defaults to \code{"unique"} See 
#' \code{tibble::\link[tibble:as_tibble]{as_tibble}} for details.
#' @inheritParams read_rds
#' @importFrom haven read_dta is.labelled
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom fs path_ext_remove path_file is_file
#' @importFrom labelled var_label
#' @importFrom dplyr bind_cols select_if mutate_all select
#' @importFrom tidyselect all_of
#' @importFrom purrr safely
#' @return A tibble.
#'
#'   Variable labels are stored in the "label" attribute of each variable.
#'   It is not printed on the console, but the RStudio viewer will show it.
#'
#'   `write_sav()` returns the input `data` invisibly.
#' @name read_dta
#' @family import functions
#' @examples
#' \donttest{
#' path <- system.file("examples", "iris.dta", package = "haven")
#' read_dta(path)
#' }
#' @export
 
read_dta <- function(file, 
                      id = NULL, 
                      doi = NULL, 
                      .name_repair = "unique") {
  
  source_file_info <- valid_file_info(file)
  
  safely_read_haven_dta  <- purrr::safely(.f = haven::read_dta)

  tmp <- safely_read_haven_dta (file = file, 
                                .name_repair = .name_repair)
  
  if ( ! is.null(tmp$error) ) {
    warning ( tmp$error, "\nReturning an empty survey." )
    return(
      survey ( data.frame(), id="Could not read file", filename=fs::path_file(file), doi=doi)
    )
  } else {
    tmp  <- tmp$result
  }
  
  tmp <- tmp %>%
    tibble::rowid_to_column()
  
  all_vars <- names(tmp)
  
  assertthat::assert_that(length(all_vars)>0, 
                          msg = "The STATA file has no names.")
  
  filename <- fs::path_file(file)
  
  if ( is.null(id) ) {
    id <- fs::path_ext_remove ( filename )
  }
  
  if ( is.null(doi)) {
    if ( "doi" %in% names(tmp) ) {
      doi <- tmp$doi[1]
    }
  }
  
  tmp$rowid <- paste0(id, "_", tmp$rowid)
  
 
  label_orig <- lapply ( tmp, labelled::var_label  )
 
  labelled::var_label ( 
    tmp$rowid ) <- paste0("Unique identifier in ", id)

  converted <- tmp [ ! vapply ( tmp, function(x) is.null(attr(x, "labels")), logical(1)) ]
  converted <- converted  [ ! vapply ( converted , function(x) length(attr(x, "labels"))>0, logical(1)) ]
  

  converted <- converted %>%
    mutate_all ( as_labelled_spss_survey, id )
  
  not_converted <- tmp %>%
    select( -all_of(names(converted)))
  
  convert_fake_labelled <- function (x) {
    
    # Fake labelled cases do not have labels, and they confuse the labelling functions
    # They should be immediately imported as non-labelled vectors 
    
    if (! inherits(x, "haven_labelled")) { return (x)}
    
    if ( inherits(x, "double") & length(attr(x, "labels"))==0) {
      as_numeric(x)
    } else if ( inherits(x, "character") & length(attr(x, "labels"))==0 ) {
      as_character(x)
    } else {
      x
    }
  }
  
  not_converted <- not_converted %>%
    mutate_all ( convert_fake_labelled )
  
  if ( ncol(converted)==0 ) {
    return_df <- not_converted
  } else if ( ncol(not_converted) == 0 ) {
    return_df <- converted
  } else {
    return_df <- bind_cols (not_converted, converted)
  }

  return_df <- return_df  %>%
    select ( all_of(all_vars) )
  
  names ( return_df )
  
  labelling_orig <- names  ( label_orig )
  labelling_orig[as.numeric(which(  vapply ( label_orig, is.null, logical(1)))) ] <- ""
  labelling_orig 
  
  original_labels <- c(
    list ( rowid = "Unique ID"), 
    lapply ( label_orig, function(x) ifelse(is.null(x), "", x))
    
  )  

  for ( i in seq_along(return_df ) ) {
    ## only labellled classes will have a label
    labelled::var_label ( return_df[,1] ) <- unlist(original_labels)[i]
  }
  
  return_survey <- survey (return_df, id=id, filename=filename, doi=doi)
  
  object_size <- as.numeric(object.size(as_tibble(return_df)))
  attr(return_survey, "object_size") <- object_size
  attr(return_survey, "source_file_size") <- source_file_info$size
  
  return_survey
}
