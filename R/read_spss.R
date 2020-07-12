#' Read SPSS (`.sav`, `.zsav`, `.por`) files. Write `.sav` and `.zsav` files.
#'
#' `read_sav()` reads both `.sav` and `.zsav` files; `write_sav()` creates
#' `.zsav` files when `compress = TRUE`. `read_por()` reads `.por` files.
#' `read_spss()` uses either `read_por()` or `read_sav()` based on the
#' file extension.
#'
#' This is a wrapper around \code{haven::\link[haven:read_spss]{read_spss}} 
#'
#' @inheritParams haven::read_spss
#' @param id An identifier of the tibble, if omitted, defaults to the
#' file name.
#' @param doi An optional document object identifier.
#' @param filename An import file name.
#' @importFrom haven read_spss read_sav write_sav
#' @importFrom tibble rowid_to_column
#' @return A tibble, data frame variant with nice defaults.
#'
#'   Variable labels are stored in the "label" attribute of each variable.
#'   It is not printed on the console, but the RStudio viewer will show it.
#'
#'   `write_sav()` returns the input `data` invisibly.
#' @name read_spss
#' @family import functions
#' @examples
#' path <- system.file("examples", "iris.sav", package = "haven")
#' haven::read_sav(path)
#'
#' tmp <- tempfile(fileext = ".sav")
#' haven::write_sav(mtcars, tmp)
#' haven::read_sav(tmp)
#' @export


read_spss <- function(file, user_na = NULL,
                      col_select= NULL, skip = NULL,
                      n_max=NULL, .name_repair = "unique",
                      id = NULL, 
                      filename = NULL, 
                      doi = NULL) {

  # how to pass on optional parameters?

  tmp <- haven::read_spss (file = file,
                    .name_repair = .name_repair) %>%
    tibble::rowid_to_column()

  filename <- fs::path_file(path)

  tmp$rowid <- paste0(id, "_", tmp$rowid)

  survey (tmp, id=id, filename=filename, doi=doi)
}
