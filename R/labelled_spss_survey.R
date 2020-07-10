## A recreation and augmentation of the haven_labelled_spss class -----------------

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
  ## vec_cast_names in utils
  na_values <- vec_cast_named(na_values, x, 
                              x_arg = "na_values", to_arg = "x")
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

  tmp <- haven::labelled_spss(x,
                       labels = labels,
                       label = label,
                       na_values = na_values,
                       na_range = na_range)
  
  attr(tmp, "id") <- id
  attr(tmp, "class") <- c("retroharmonize_labelled_spss_survey",
                                 "haven_labelled_spss")
 tmp
}

## Displaying methods -----------------------------------------
get_labeltext <- function(x, prefix=": ") {
  label = attr(x, "label", exact = TRUE)
  if(!is.null(label)) {
    paste0(prefix, label)
  }
}

#' @export
vec_ptype_full.retroharmonize_labelled_spss_survey <- function(x, ...) {
  paste0("labelled_survey<", vec_ptype_full(vec_data(x)), ">")
}

#' @export
vec_ptype_abbr.retroharmonize_labelled_spss_survey <- function(x, ...) {
 
  if ( vec_ptype_full(vec_data(x)) == "character" ) {
    "retroh_chr"
  } else if ( vec_ptype_full(vec_data(x)) == "integer") {
    "retroh_int"
  } else if (vec_ptype_full(vec_data(x)) == "double") {
    "retroh_dbl"
  } else {
    "retroh"
  }
}
#' @export
 
#' @export
obj_print_header.retroharmonize_labelled_spss_survey <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x))
  invisible(x)
}


obj_print_footer.retroharmonize_labelled_spss_survey <- function(x, ...) {
  
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

## Missingness --------------------------------------

#' @export
is.na.retroharmonize_labelled_spss_survey <- function(x) {
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



## Coercion rules --------------------------------------

#' @export
as.character.retroharmonize_labelled_spss_survey <- function(x, ...) {
  as.character(vec_data(x))
}

#' @export
levels.retroharmonize_labelled_spss_survey <- function(x) {
  NULL
}

`names<-.retroharmonize_labelled_spss_survey` <- function(x, value) {
  attr(x, "names") <- value
  x
}

#' @importFrom haven format_tagged_na
#' @export
format.retroharmonize_labelled_spss_survey <- function(x, ..., digits = getOption("digits")) {
  if (is.double(x)) {
    haven::format_tagged_na(x, digits = digits)
  } else {
    format(vec_data(x), ...)
  }
}

# Type system -------------------------------------------------------------

# Import to avoid R CMD check NOTE
#' @importFrom methods setOldClass
setOldClass(c("retroharmonize_labelled_spss_survey", 
              "haven_labelled_spss", 
              "haven_labelled", "vctrs_vctr"))

#' @export
#' @rdname labelled_spss_survey
is.labelled_spss_survey <- function(x) {
  inherits(x, "retroharmonize_labelled_spss_survey")
}

#' @export
vec_ptype2.retroharmonize_labelled_spss_survey.double <- function(x, y, ...) double()
vec_ptype2.double.retroharmonize_labelled_spss_survey <- function(x, y, ...) double()

#' @export
vec_ptype2.integer.retroharmonize_labelled_spss_survey <- function(x, y, ...) double()
vec_ptype2.retroharmonize_labelled_spss_survey.integer <- function(x, y, ...) double()


#' @export
vec_cast.double.retroharmonize_labelled_spss_survey  <- function(x, to, ...) vec_cast(vec_data(x), to)
#' @export
vec_cast.integer.retroharmonize_labelled_spss_survey  <- function(x, to, ...) vec_cast(vec_data(x), to)
#' @export
vec_cast.character.retroharmonize_labelled_spss_survey  <- function(x, to, ...) {
  if (is.character(x)) {
    vec_cast(vec_data(x), to, ...)
  } else {
    stop_incompatible_cast(x, to, ...)
  }
}

## Artithmetics ------------------------------------------------
vec_convert_na <- function(x) {
  
  ## na range is not implemented
  
  ifelse ( vec_data(x) %in% labelled::na_values(x), 
           NA_real_, 
           vec_data(x))
}

#' @importFrom stats median
median.retroharmonize_labelled_spss_survey <- function(x, na.rm = TRUE, ...) {
  if (is.character(x)) {
    abort("Can't compute median of labelled_spss_survey<character>")
  }
  
  median(vec_convert_na(x), na.rm = TRUE, ...)
}

#' @importFrom stats quantile
#' @export
quantile.retroharmonize_labelled_spss_survey <- function(x, probs, ...) {
  if (is.character(x)) {
    abort("Can't compute median of labelled_spss_survey<character>")
  }
  quantile(vec_convert_na(x), probs, na.rm = TRUE, ...)
}

#' @importFrom stats weighted.mean
weighted.mean.retroharmonize_labelled_spss_survey <- function(x, w, ...) {
  if (is.character(x)) {
    abort("Can't compute median of labelled_spss_survey<character>")
  }
  
  if (! is.numeric(w)) {
    abort("Weights must be numeric.")
  }
  
  weighted.mean(vec_convert_na(x), w, na.rm = TRUE, ...)
}

mean.retroharmonize_labelled_spss_survey <- function(x, ...) {
  if (is.character(x)) {
    abort("Can't compute mean of labelled_spss_survey<character>")
  }

  mean(vec_convert_na(x), ...)
}


sum.retroharmonize_labelled_spss_survey <- function(x, ...) {
  if (is.character(x)) {
    abort("Can't compute sum of labelled_spss_survey<character>")
  }
  sum(vec_convert_na(x), ...)
}

#' @importFrom haven as_factor
#' @export
#' @rdname labelled_spss_survey
#' @family type conversion functions
as_factor <- function(x) {
  haven::as_factor(x) 
  }

#' @family type conversion functions
#' @rdname labelled_spss_survey
#' @export
as_numeric <- function(x) {
  vec_convert_na(x) 
  }