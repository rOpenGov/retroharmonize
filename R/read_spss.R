#' Read SPSS (`.sav`, `.zsav`, `.por`) files. Write `.sav` and `.zsav` files.
#'
#' `read_sav()` reads both `.sav` and `.zsav` files; `write_sav()` creates
#' `.zsav` files when `compress = TRUE`. `read_por()` reads `.por` files.
#' `read_spss()` uses either `read_por()` or `read_sav()` based on the
#' file extension.
#'
#' This is a wrapper around \code{haven::\link[haven:read_spss]{read_spss}} 
#'
#' @param file An SPSS file.
#' @param user_na Should user-defined na_values be imported? Defaults
#' to \code{TRUE}.
#' @param .name_repair Defaults to \code{"unique"} See 
#' \code{tibble::\link[tibble:as_tibble]{as_tibble}} for details.
#' @inheritParams read_rds
#' @importFrom haven read_spss read_sav write_sav is.labelled
#' @importFrom tibble rowid_to_column
#' @importFrom fs path_ext_remove path_file
#' @importFrom labelled var_label
#' @importFrom dplyr bind_cols select_if mutate_all select
#' @importFrom tidyselect all_of
#' @return A tibble, data frame variant with nice defaults.
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
                      id = NULL, 
                      filename = NULL, 
                      doi = NULL, 
                      .name_repair = "unique") {
  
  # to do: with ...
  #skip = NULL,
  #col_select = NULL 
  #n_max =NULL
  #col_select_input <- col_select
  
  if (! file.exists(file) ) stop ("The file does not exist.")

  # how to pass on optional parameters?

  tmp <- haven::read_spss (file = file, 
                           user_na = user_na, 
                           .name_repair = .name_repair)
  
  tmp <- tmp %>% tibble::rowid_to_column()
  
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
  labelled::var_label ( 
    tmp$rowid ) <- paste0("Unique identifier in ", id)
  
  all_vars <- names(tmp)
  
  converted <- tmp %>%
    select_if( haven::is.labelled ) %>%
    mutate_all ( as_labelled_spss_survey, id )
  
  not_converted <- tmp %>%
    select( -all_of(names(converted)))
  
  tmp <- bind_cols ( not_converted, converted ) %>%
    select ( all_of(all_vars) )

  survey (tmp, id=id, filename=filename, doi=doi)
}
