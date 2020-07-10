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

# ------------------------------------------------------------------------------
# Registration function
# Copied from googledrive r package, dplyr-compat.R

## function is called in .onLoad()

# nocov start

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg))
  envir <- asNamespace(pkg)
  
  stopifnot(is.character(generic))
  stopifnot(is.character(class))
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}


#.onLoad <- function(libname, pkgname) {
  
  # If tidyr is available, library() it and register these methods implemented
  # in tibbletime.
  # This is done because tidyr is not imported because it is not used
  # anywhere else in the package.
#  if (requireNamespace("haven", quietly = TRUE)) {
#    register_s3_method("haven", "as_factor", "labelled_spss_survey")
    #tidyr_at_least_1.0.0 <<- utils::packageVersion("tidyr") >= "1.0.0"
#  }
  
#  invisible()
#}