## Internal functions not to be exported ----------------------

vec_cast_named <- function(x, to, ...) {
  #identical to haven:::vec_cast_named()
  stats::setNames(vec_cast(x, to, ...), names(x))
}

cat_line <- function(...) {
  # identical to haven:::cat_line
  cat(paste0(..., "\n", collapse = ""))
}

is.labelled_spss <- function (x) inherits(x, "haven_labelled_spss")
is.labelled <- function (x) inherits(x, "haven_labelled")

#' Convert to haven_labelled_spss
#' @param x A vector
#' @param na_labels A named vector of missing values, defaults to
#' \code{c( "inap" = "inap")} for character vectors and
#' \code{c( 99999 = "inap")} for numeric vectors.
#' @return A haven_labelled_spss vector
#' @importFrom labelled val_labels
#' @keywords internal

convert_to_labelled_spss <- function(x, na_labels = NULL ) {

  if ( is.null(na_labels) && is.numeric(unclass(x))) {
    na_labels <- structure(99999, names = "inap")
  } else if (is.null(na_labels) && is.character(unclass(x))) {
    na_labels <- structure("inap", names = "inap")
  }

  stopifnot( ! any(unclass(na_labels) %in% x) )


  labelled_spss(x, c( labelled::val_labels (x), na_labels),
                na_values = unclass(na_labels))
}

