#' Coercion methods for labelled survey vectors
#'
#' Convert labelled SPSS-style survey vectors to common R data types.
#' These helpers provide consistent coercion behavior for
#' `"retroharmonize_labelled_spss_survey"` objects while respecting
#' labelled missing values.
#'
#' @param x A labelled survey vector created with
#'   [labelled_spss_survey()].
#'
#' @return
#' * `as_numeric()` returns a numeric vector with labelled missing
#'   values converted to `NA`.
#' * `as_character()` returns a character vector based on the
#'   factor representation of `x`.
#' * `as_factor()` returns a factor with levels derived from value
#'   labels.
#'
#' @seealso
#' [labelled_spss_survey()],
#' [haven::as_factor()]
#'
#' @family type conversion functions
#'
#' @name labelled_spss_survey_coercion
NULL


#' @rdname labelled_spss_survey_coercion
#' @export
as_numeric <- function(x) {
  vec_convert_na(x)
}


#' @rdname labelled_spss_survey_coercion
#' @export
as_character <- function(x) {
  as.character(as_factor(x))
}


#' @rdname labelled_spss_survey_coercion
#' @param levels Character string indicating how factor levels
#'   should be constructed. Currently retained for compatibility.
#' @param ordered Logical; whether the resulting factor should be ordered.
#'   Currently ignored.
#' @export
as_factor <- function(x, levels = "default", ordered = FALSE) {
  tmp <- haven::as_factor(x)
  attr(tmp, "levels") <- levels(tmp)
  class(tmp) <- "factor"
  tmp
}

