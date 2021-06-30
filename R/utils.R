## Internal functions not to be exported ----------------------

#' @keywords  internal
cat_line <- function(...) {
  # identical to haven:::cat_line
  cat(paste0(..., "\n", collapse = ""))
}
#' @keywords  internal
is.labelled_spss <- function (x) inherits(x, "haven_labelled_spss")
#' @keywords  internal
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


#' @keywords  internal
remove_na_range <- function (x) {
  assert_that(is.labelled_spss_survey(x))
  if (! is.null(attr(x, "na_range")) ) {
    
    min_na_range <- min (attr(x, "na_range"))
    max_na_range <- max (attr(x, "na_range"))
    
    if ( all(! max_na_range %in% range ( x ), 
        ! min_na_range %in% range ( x )  )) {
      attr(x, "na_range") <- NULL
    }
  }
  x
}
