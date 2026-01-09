#' Convert labelled missing values to NA
#'
#' Internal helper used by numeric summary methods to replace
#' SPSS-style missing values with `NA`.
#'
#' @param x A labelled survey vector.
#'
#' @return A numeric vector with missing values converted to `NA`.
#'
#' @keywords internal
#' @importFrom labelled na_values
#' @importFrom vctrs vec_data
vec_convert_na <- function(x) {
  ifelse(
    vctrs::vec_data(x) %in% labelled::na_values(x),
    NA_real_,
    vctrs::vec_data(x)
  )
}

#' @rdname labelled_spss_survey
#' @importFrom stats median
#' @inheritParams stats::median
#' @export
median.retroharmonize_labelled_spss_survey <- function(x, 
                                                       na.rm = TRUE, 
                                                       ...) {
  if (is.character(x)) {
    abort("Can't compute median of labelled_spss_survey<character>")
  }
  
  median(vec_convert_na(x), na.rm = na.rm, ...)
}


#' @rdname labelled_spss_survey
#' @importFrom stats quantile
#' @inheritParams stats::quantile
#' @export
quantile.retroharmonize_labelled_spss_survey <- function(x, 
                                                         probs, 
                                                         ...) {
  if (is.character(x)) {
    abort("Can't compute quantiles of labelled_spss_survey<character>")
  }
  
  quantile(vec_convert_na(x), probs, na.rm = TRUE, ...)
}


#' @rdname labelled_spss_survey
#' @importFrom stats weighted.mean
#' @inheritParams stats::weighted.mean
#' @export
weighted.mean.retroharmonize_labelled_spss_survey <- function(x, 
                                                              w, 
                                                              ...) {
  if (is.character(x)) {
    abort("Can't compute weighted mean of labelled_spss_survey<character>")
  }
  
  if (!is.numeric(w)) {
    abort("Weights must be numeric.")
  }
  
  weighted.mean(vec_convert_na(x), w, na.rm = TRUE, ...)
}


#' @rdname labelled_spss_survey
#' @inheritParams base::mean
#' @export
mean.retroharmonize_labelled_spss_survey <- function(x, ...) {
  if (is.character(x)) {
    abort("Can't compute mean of labelled_spss_survey<character>")
  }
  
  mean(vec_convert_na(x), ...)
}


#' @rdname labelled_spss_survey
#' @export
sum.retroharmonize_labelled_spss_survey <- function(x, ...) {
  if (is.character(x)) {
    abort("Can't compute sum of labelled_spss_survey<character>")
  }
  
  sum(vec_convert_na(x), ...)
}

