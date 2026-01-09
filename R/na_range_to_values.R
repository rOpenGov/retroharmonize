#' Harmonize SPSS-style missing value ranges
#'
#' Ensure consistency between SPSS-style missing value ranges
#' (`na_range`) and explicit missing values (`na_values`)
#' for labelled survey vectors.
#'
#' When both attributes are present, this function:
#' \itemize{
#'   \item adjusts the missing range if it conflicts with existing missing values,
#'   \item derives missing values from the range when necessary,
#'   \item leaves non-SPSS-labelled vectors unchanged.
#' }
#'
#' This harmonization is important before joining, binding, or
#' summarizing survey data.
#'
#' @param x A labelled vector created with
#'   [haven::labelled_spss()] or
#'   `retroharmonize_labelled_spss_survey`.
#'
#' @return
#' The input vector with harmonized `na_values` and `na_range`
#' attributes. If no harmonization is needed, `x` is returned
#' unchanged.
#'
#' @seealso
#' [labelled::na_range()],
#' [labelled::na_values()],
#' [as_numeric()]
#'
#' @family variable label harmonization functions
#' @export
na_range_to_values <- function(x) {
  if (!inherits(x, "haven_labelled_spss")) {
    return(x)
  }
  if (is.null(labelled::na_range(x))) {
    return(x)
  }
  
  na_values <- numeric(0)
  
  if (!is.null(labelled::na_values(x))) {
    na_values <- labelled::na_values(x)
  }
  
  na_min <- labelled::na_range(x)[1]
  na_max <- labelled::na_range(x)[2]
  
  if (length(na_values) > 0) {
    if (min(na_values) < na_min) {
      warning("Inconsistent missing ranges: min(na_values) < min(na_range)")
      na_min <- min(na_values)
    }
    
    if (max(na_values) > na_max) {
      warning("Inconsistent missing ranges: max(na_values) > max(na_range)")
      na_max <- max(na_values)
    }
  }
  
  present_na <- unclass(x)[unclass(x) >= na_min & unclass(x) <= na_max]
  
  if (length(present_na) == 0) {
    present_na <- na_min:na_max
  }
  
  labelled::na_values(x) <- present_na
  labelled::na_range(x) <- c(na_min, na_max)
  
  x
}

#' Test whether missing values need harmonization
#'
#' Checks whether both `na_values` and `na_range` attributes
#' are present on a labelled vector.
#'
#' @param x A labelled vector.
#' @return Logical scalar.
#'
#' @keywords internal
#' @export
is.na_range_to_values <- function(x) {
  !is.null(labelled::na_values(x)) &&
    !is.null(labelled::na_range(x))
}

