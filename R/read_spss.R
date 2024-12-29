#' @title Read SPSS (`.sav`, `.zsav`, `.por`) files. Write `.sav` and `.zsav` files.
#' 
#' @description This is a wrapper around \code{haven::\link[haven:read_spss]{read_spss}} 
#' with some exception handling.
#'
#' @details `read_sav()` reads both `.sav` and `.zsav` files; `write_sav()` creates
#' `.zsav` files when `compress = TRUE`. `read_por()` reads `.por` files.
#' `read_spss()` uses either `read_por()` or `read_sav()` based on the
#' file extension.
#'
#' When the SPSS file has columns which are of class labelled, but have no labels,
#' they are read as numeric or character vectors. 
#'
#' @param file An SPSS file.
#' @param user_na Should user-defined na_values be imported? Defaults
#' to \code{TRUE}.
#' @param dataset_bibentry A bibliographic entry created with 
#' \code{dataset::\link[dataset:dublincore]{dublincore}} or 
#' \code{dataset::\link[dataset:datacite]{datacite}}.
#' @param .name_repair Defaults to \code{"unique"} See 
#' \code{tibble::\link[tibble:as_tibble]{as_tibble}} for details.
#' @inheritParams read_rds
#' @importFrom haven read_spss read_sav write_sav is.labelled
#' @importFrom assertthat assert_that
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom fs path_ext_remove path_file is_file
#' @importFrom labelled var_label
#' @importFrom dplyr bind_cols select_if mutate_all select all_of
#' @importFrom purrr safely
#' @importFrom utils object.size
#' @return A tibble:
#'
#'   Variable labels are stored in the "label" attribute of each variable.
#'   It is not printed on the console, but the RStudio viewer will show it.
#'
#'   `write_sav()` returns the input `data` invisibly.
#' @name read_spss
#' @family import functions
#' @examples
#' \donttest{
#' path <- system.file("examples", "iris.sav", package = "haven")
#' haven::read_sav(path)
#'
#' tmp <- tempfile(fileext = ".sav")
#' haven::write_sav(mtcars, tmp)
#' haven::read_sav(tmp)
#' }
#' @export
 
read_spss <- function(file, 
                      user_na = TRUE,
                      dataset_bibentry = NULL,
                      id = NULL, 
                      doi = NULL, 
                      .name_repair = "unique") {
  
  # to do: with ...
  #skip = NULL,
  #col_select = NULL 
  #n_max =NULL
  #col_select_input <- col_select
  # how to pass on optional parameters?
  
  source_file_info <- valid_file_info(file)
  
  safely_read_haven_spss <- safely(.f = haven::read_spss)

  tmp <- safely_read_haven_spss (file = file, 
                                 user_na = user_na, 
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
  
  assert_that(length(all_vars)>0, 
              msg = "The SPSS file has no names.")
  
  filename <- path_file(file)
  
  if ( is.null(id) ) {
    id <- path_ext_remove(filename)
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
  
  return_survey <- survey_df(return_df, 
                             dataset_bibentry = dataset_bibentry, 
                             filename=filename, identifier=doi)
  
  object_size <- as.numeric(object.size(as_tibble(return_df)))
  attr(return_survey, "object_size")      <- object_size
  attr(return_survey, "source_file_size") <- source_file_info$size

  
  if (dataset::dataset_title(return_survey)=="Untitled Dataset") {
    dataset::dataset_title(return_survey, overwrite=TRUE) <- "Untitled Survey" 
  }
  
  ## For backward compatibility
  attr(return_survey, "id") <- id
  attr(return_survey, "doi") <- doi
  
  return_survey
}
