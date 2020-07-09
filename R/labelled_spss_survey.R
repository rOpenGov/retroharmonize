#' Labelled vectors for SPSS surveys
#'
#' This class is only used when `user_na = TRUE` in
#' [read_sav()]. It is similar to the [labelled()] class
#' but it also models SPSS's user-defined missings, which can be up to
#' three distinct values, or for numeric vectors a range.
#'
#' @param id Survey ID
#' @importFrom haven labelled labelled_spss
#' @inheritParams haven::labelled_spss
#' @export
#' @examples
#' x1 <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
#' is.na(x1)
#'
#' x2 <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_range = c(9, Inf),
#'                     label = "Quality rating")
#' is.na(x2)
#'
#' # Print data and metadata
#' x2
#'

labelled_spss_survey <- function(
  x = double(), labels = NULL,
  na_values = NULL,
  na_range = NULL, label = NULL,
  id = NULL) {

  x <- vec_data(x)
  na_values <- vec_cast_named(na_values, x, x_arg = "na_values", to_arg = "x")
  labelled <- labelled(x, labels = labels, label = label)

  new_labelled_spss_survey(
    vec_data(labelled),
    labels = attr(labelled, "labels"),
    na_values = na_values,
    na_range = na_range,
    id = id,
    label = attr(labelled, "label", exact = TRUE)
  )
}

new_labelled_spss_survey <- function(x, labels,
                                     na_values, na_range,
                                     label, id) {
  if (!is.null(na_values) && !vec_is(x, na_values)) {
    abort("`na_values` must be same type as `x`.")
  }
  if (!is.null(na_range)) {
    if (!is.numeric(x)) {
      abort("`na_range` is only applicable for labelled numeric vectors.")
    }
    if (!is.numeric(na_range) || length(na_range) != 2) {
      abort("`na_range` must be a numeric vector of length two.")
    }
  }

  haven:::new_labelled(x,
                       labels = labels,
                       label = label,
                       na_values = na_values,
                       na_range = na_range,
                       id = id,
                       class = c("surveyharmonize_labelled_spss_survey",
                                 "haven_labelled_spss")
  )
}

#' @export
vec_ptype_full.surveyharmonize_labelled_spss_survey <- function(x, ...) {
  paste0("labelled_spss<", vec_ptype_full(vec_data(x)), ">")
}

#' @export
obj_print_footer.surveyharmonized_labelled_spss_survey <- function(x, ...) {
  na_values <- attr(x, "na_values")
  if (!is.null(na_values)) {
    cat_line("Missing values: ", paste(na_values, collapse = ", "))
  }

  na_range <- attr(x, "na_range")
  if (!is.null(na_range)) {
    cat_line("Missing range:  [", paste(na_range, collapse = ", "), "]")
  }

  cat_line("Survey ID:", attr(x, "id"))

  NextMethod()
}


#' @export
is.na.surveyharmonize_labelled_spss_survey <- function(x) {
  miss <- NextMethod()
  val <- vec_data(x)

  na_values <- attr(x, "na_values")
  if (!is.null(na_values)) {
    miss <- miss | val %in% na_values
  }

  na_range <- attr(x, "na_range")
  if (!is.null(na_range)) {
    miss <- miss | (val >= na_range[1] & val <= na_range[2])
  }

  miss
}


#' @export
is.surveyharmonize_labelled_spss_survey <- function(x) {
  inherits(x, "surveyharmonize_labelled_spss_survey")
}
