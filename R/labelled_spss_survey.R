#' Labelled SPSS-style vectors with survey provenance
#'
#' Create a labelled vector compatible with
#' [haven::labelled_spss()] that carries additional
#' survey-level provenance metadata.
#'
#' The resulting object behaves like a
#' `haven_labelled_spss` vector, but stores:
#' \itemize{
#'   \item a survey identifier
#'   \item the original variable name
#'   \item the original value coding
#' }
#'
#' @details
#' Several arithmetic (statistical summary) methods operate on the
#' numeric representation of labelled survey vectors, converting 
#' SPSS-style missing values to `NA`
#' before computation.
#' 
#' You can coerce labelled_spss_survey vectors to numeric, 
#' character or factor representation.
#' 
#' @param x A vector of values.
#' @param labels A named vector of value labels.
#' @param na_values A vector of values to be treated as missing.
#' @param na_range A numeric range defining missing values.
#' @param label A variable label.
#' @param id A character scalar identifying the survey.
#' @param name_orig Original variable name. Defaults to the name of `x`.
#'
#' @return
#' An object of class `"retroharmonize_labelled_spss_survey"`,
#' extending [haven::labelled_spss()].
#'
#' @seealso
#' [haven::labelled_spss()],
#' [as_factor()],
#' [as_numeric()], 
#' [as_character()]
#'
#' @importFrom labelled labelled
#' @importFrom vctrs vec_data vec_cast
#'
#' @examples
#' x <- labelled_spss_survey(
#'   x = c(1, 2, 9),
#'   labels = c(Yes = 1, No = 2),
#'   na_values = 9,
#'   id = "survey_1"
#' )
#'
#' is.na(x)
#' as_factor(x)
#'
#' @export
labelled_spss_survey <- function(
  x = double(), labels = NULL,
  na_values = NULL, na_range = NULL,
  label = NULL, id = NULL, name_orig = NULL
) {
  x_vector <- vctrs::vec_data(x)
  vec_cast_named <- function(x, to, ...) {
    # identical to haven:::vec_cast_named()
    stats::setNames(vctrs::vec_cast(x, to, ...), names(x))
  }
  na_values <- vec_cast_named(na_values, x_vector,
    x_arg = "na_values", to_arg = "x"
  )
  labelled <- labelled::labelled(x, labels = labels)

  if (is.null(name_orig)) {
    name_orig <- deparse(substitute(x))
  }

  if (is.null(id)) id <- name_orig

  tmp <- new_labelled_spss_survey(
    vctrs::vec_data(labelled),
    labels = labels,
    label = label,
    na_values = na_values,
    na_range = na_range,
    id = id,
    name_orig = name_orig
  )

  tmp
}

#' @importFrom vctrs vec_is
#' @importFrom haven labelled_spss
#' @keywords internal
new_labelled_spss_survey <- function(x, 
                                     labels,
                                     na_values, 
                                     na_range,
                                     label, 
                                     id,
                                     name_orig) {
  if (!is.null(na_values) && !vctrs::vec_is(x, na_values)) {
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

  tmp <- haven::labelled_spss(vec_data(x),
    labels = labels,
    label = label,
    na_values = na_values,
    na_range = na_range
  )

  original_coding <- sort(unique(x))
  names(original_coding) <- original_coding

  attr(tmp, "class") <- c(
    "retroharmonize_labelled_spss_survey",
    "haven_labelled_spss",
    "haven_labelled"
  )
  if ((length(id) == 1)) {
    attr(tmp, paste0(id, "_name")) <- name_orig
    attr(tmp, paste0(id, "_values")) <- original_coding
    attr(tmp, paste0(id, "_label")) <- label
    attr(tmp, paste0(id, "_labels")) <- attr(tmp, "labels")
    attr(tmp, paste0(id, "_na_values")) <- attr(tmp, "na_values")
    attr(tmp, paste0(id, "_na_range")) <- attr(tmp, "na_range")
  } else {
    id <- paste(id, collapse = ", ")
  }

  attr(tmp, "id") <- id

  tmp
}

## Subsetting -------------------------------------------------

#' @rdname labelled_spss_survey
#' @param i Index vector used for subsetting.
#' @export
`[.retroharmonize_labelled_spss_survey` <- function(x, i, ...) {
  preserve_structure <- attributes(x)
  x <- vec_data(x)[i]
  attributes(x) <- preserve_structure
  x
}

## Utility ----------------------------------------------------
#' @keywords internal
vec_cast_named <- function(x, to, ...) {
  # identical to haven:::vec_cast_named()
  stats::setNames(vctrs::vec_cast(x, to, ...), names(x))
}

## Displaying methods -----------------------------------------
#' @keywords internal
get_labeltext <- function(x, prefix = ": ") {
  label <- attr(x, "label", exact = TRUE)
  if (!is.null(label)) {
    paste0(prefix, label)
  }
}

#' Labelled SPSS-style vectors with survey provenance
#'
#' Create a labelled vector compatible with
#' [haven::labelled_spss()] that carries additional
#' survey-level provenance metadata.
#'
#' The resulting object behaves like a
#' `haven_labelled_spss` vector, but stores:
#' \itemize{
#'   \item a survey identifier
#'   \item the original variable name
#'   \item the original value coding
#' }
#'
#' @details
#' Several arithmetic (statistical summary) methods operate on the
#' numeric representation of labelled survey vectors, converting 
#' SPSS-style missing values to `NA`
#' before computation.
#' 
#' You can coerce labelled_spss_survey vectors to numeric, 
#' character or factor representation.
#' 
#' @param x A vector of values.
#' @param labels A named vector of value labels.
#' @param na_values A vector of values to be treated as missing.
#' @param na_range A numeric range defining missing values.
#' @param label A variable label.
#' @param id A character scalar identifying the survey.
#' @param name_orig Original variable name. Defaults to the name of `x`.
#'
#' @return
#' An object of class `"retroharmonize_labelled_spss_survey"`,
#' extending [haven::labelled_spss()].
#'
#' @seealso
#' [haven::labelled_spss()],
#' [as_factor()],
#' [as_numeric()], 
#' [as_character()]
#'
#' @examples
#' x <- labelled_spss_survey(
#'   x = c(1, 2, 9),
#'   labels = c(Yes = 1, No = 2),
#'   na_values = 9,
#'   id = "survey_1"
#' )
#'
#' is.na(x)
#' as_factor(x)
#'
#' @export
labelled_spss_survey <- function(
  x = double(), labels = NULL,
  na_values = NULL, na_range = NULL,
  label = NULL, id = NULL, name_orig = NULL
) {
  x_vector <- vctrs::vec_data(x)
  vec_cast_named <- function(x, to, ...) {
    # identical to haven:::vec_cast_named()
    stats::setNames(vctrs::vec_cast(x, to, ...), names(x))
  }
  na_values <- vec_cast_named(na_values, x_vector,
    x_arg = "na_values", to_arg = "x"
  )
  labelled <- labelled::labelled(x, labels = labels)

  if (is.null(name_orig)) {
    name_orig <- deparse(substitute(x))
  }

  if (is.null(id)) id <- name_orig

  tmp <- new_labelled_spss_survey(
    vctrs::vec_data(labelled),
    labels = labels,
    label = label,
    na_values = na_values,
    na_range = na_range,
    id = id,
    name_orig = name_orig
  )

  tmp
}

#' @keywords internal
new_labelled_spss_survey <- function(x, 
                                     labels,
                                     na_values, 
                                     na_range,
                                     label, 
                                     id,
                                     name_orig) {
  if (!is.null(na_values) && !vctrs::vec_is(x, na_values)) {
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

  tmp <- haven::labelled_spss(vec_data(x),
    labels = labels,
    label = label,
    na_values = na_values,
    na_range = na_range
  )

  original_coding <- sort(unique(x))
  names(original_coding) <- original_coding

  attr(tmp, "class") <- c(
    "retroharmonize_labelled_spss_survey",
    "haven_labelled_spss",
    "haven_labelled"
  )
  if ((length(id) == 1)) {
    attr(tmp, paste0(id, "_name")) <- name_orig
    attr(tmp, paste0(id, "_values")) <- original_coding
    attr(tmp, paste0(id, "_label")) <- label
    attr(tmp, paste0(id, "_labels")) <- attr(tmp, "labels")
    attr(tmp, paste0(id, "_na_values")) <- attr(tmp, "na_values")
    attr(tmp, paste0(id, "_na_range")) <- attr(tmp, "na_range")
  } else {
    id <- paste(id, collapse = ", ")
  }

  attr(tmp, "id") <- id

  tmp
}

## Subsetting -------------------------------------------------

#' @rdname labelled_spss_survey
#' @export
`[.retroharmonize_labelled_spss_survey` <- function(x, i, ...) {
  preserve_structure <- attributes(x)
  x <- vec_data(x)[i]
  attributes(x) <- preserve_structure
  x
}

## Utility ----------------------------------------------------
#' @keywords internal
vec_cast_named <- function(x, to, ...) {
  # identical to haven:::vec_cast_named()
  stats::setNames(vctrs::vec_cast(x, to, ...), names(x))
}

## Displaying methods -----------------------------------------
#' @keywords internal
get_labeltext <- function(x, prefix = ": ") {
  label <- attr(x, "label", exact = TRUE)
  if (!is.null(label)) {
    paste0(prefix, label)
  }
}

#' @keywords internal
print_attributes <- function(x, full = TRUE) {
  na_values <- attr(x, "na_values")
  if (!is.null(na_values)) {
    cli::cat_line("Missing values: ", paste(na_values, collapse = ", "))
  }
  na_range <- attr(x, "na_range")
  if (!is.null(na_range)) {
    cli::cat_line("Missing range:  [", paste(na_range, collapse = ", "), "]")
  }

  if (full == FALSE) invisible(x)
  ## full printing goes on below ---------------------

  history_attributes <- names(attributes(x))
  history_attributes <- history_attributes[
    !history_attributes %in% c(
      "label", "labels",
      "na_values", "na_range", "class", "id"
    )
  ]

  if (length(history_attributes) > 0) {
    last_attribute <- history_attributes[length(history_attributes)]
    history_attributes <- c(history_attributes[1:3], "...", last_attribute)
    history_attributes <- paste(history_attributes, collapse = ", ")
    history_attributes <- gsub("\\,\\s\\.\\.\\.", " [...]", history_attributes)
  }

  cli::cat_line(paste0(
    "See all attributes ",
    history_attributes,
    " with attributes(",
    deparse(substitute(x)),
    ")"
  ))
}

#' @export
vec_ptype_full.retroharmonize_labelled_spss_survey <- function(x, ...) {
  paste0("labelled_spss_survey<", vctrs::vec_ptype_full(vctrs::vec_data(x)), ">")
  cli::cat_line("Survey ID: ", attr(x, "id"))
}

#' @export
vec_ptype_abbr.retroharmonize_labelled_spss_survey <- function(x, ...) {
  if (vec_ptype_full(vec_data(x)) == "character") {
    "retroh_chr"
  } else if (vec_ptype_full(vctrs::vec_data(x)) == "integer") {
    "retroh_int"
  } else if (vec_ptype_full(vctrs::vec_data(x)) == "double") {
    "retroh_dbl"
  } else {
    "retroh"
  }
}
#' @export

#' @export
#' @importFrom cli cat_line
obj_print_header.retroharmonize_labelled_spss_survey <- function(x, ...) {
  cli::cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x))
  invisible(x)
}

#' @export
obj_print_footer.retroharmonize_labelled_spss_survey <- function(x, ...) {
  print_attributes(x)
  invisible(x)
}

#' @rdname labelled_spss_survey
#' @importFrom cli cat_line
#' @importFrom utils head
print.retroharmonize_labelled_spss_survey <- function(x, ...) {
  cli::cat_line("<", vec_ptype_full(x), 
                "[", vec_size(x), "]>", get_labeltext(x))
  cat(head(vec_data(x), 20))
  cat("\n")
  print_attributes(x)
  invisible(x)
}

## Usual methods  ------------------------------------------------

#' @rdname labelled_spss_survey
#' @param object A labelled_spss_survey to summarize.
#' @export
summary.retroharmonize_labelled_spss_survey <- function(object, ...) {
  if (!is.null(attr(object, "label"))) {
    cat(attr(object, "label"))
  }
  cat("\nNumeric values without coding:\n")
  print(summary(vec_data(object), ...))
  cat("Numeric representation:\n")
  print(summary(as_numeric(object)))
  cat("Factor representation:\n")
  summary(as_factor(object))
}

## Missingness --------------------------------------

#' @rdname labelled_spss_survey
#' @export
is.na.retroharmonize_labelled_spss_survey <- function(x) {
  miss <- NextMethod()
  val <- vctrs::vec_data(x)

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

#' @rdname labelled_spss_survey
#' @exportS3Method
levels.retroharmonize_labelled_spss_survey <- function(x) {
  NULL
}

#' @rdname labelled_spss_survey
#' @param value Replacement values used when assigning names.
#' @export
"names<-.retroharmonize_labelled_spss_survey" <- function(x, value) {
  attr(x, "names") <- value
  x
}

#' @rdname labelled_spss_survey
#' @importFrom haven format_tagged_na
#' @export
format.retroharmonize_labelled_spss_survey <- function(x, ..., digits = getOption("digits")) {
  if (is.double(x)) {
    haven::format_tagged_na(x, digits = digits)
  } else {
    format(vctrs::vec_data(x), ...)
  }
}

# Type system -------------------------------------------------------------

# Import to avoid R CMD check NOTE
#' @importFrom methods setOldClass
# setOldClass(c("retroharmonize_labelled_spss_survey",
#              "haven_labelled_spss",
#              "haven_labelled", "vctrs_vctr"))


#' @rdname labelled_spss_survey
#' @export
is.labelled_spss_survey <- function(x) {
  inherits(x, "retroharmonize_labelled_spss_survey")
}

#' vctrs type and casting methods for labelled survey vectors
#'
#' These methods define how
#' \code{retroharmonize_labelled_spss_survey} objects interact with
#' base vectors and with each other in vctrs-based operations such as
#' concatenation, binding, and coercion.
#'
#' They ensure that labelled survey vectors:
#' \itemize{
#'   \item combine safely with numeric vectors,
#'   \item cast consistently to base types,
#'   \item error on incompatible coercions.
#' }
#'
#' These functions are part of the internal type system and are not
#' intended to be called directly by users.
#'
#' @name labelled_spss_survey_vctrs
#' @keywords internal
NULL

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.retroharmonize_labelled_spss_survey.double <- function(x, 
                                                                  y, 
                                                                  ...) {
  double() 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.double.retroharmonize_labelled_spss_survey <- function(x,
                                                                  y, 
                                                                  ...) {
  double() 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.integer.retroharmonize_labelled_spss_survey <- function(x, 
                                                                   y, 
                                                                   ...) {
  double() 
  }

vec_ptype2.retroharmonize_labelled_spss_survey.integer <- function(x, 
                                                                   y, 
                                                                   ...) {
  double() 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_cast.double.retroharmonize_labelled_spss_survey <- function(x, 
                                                                to, 
                                                                ...) {
  vec_cast(vctrs::vec_data(x), to) 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_cast.integer.retroharmonize_labelled_spss_survey <- function(x,
                                                                 to, 
                                                                 ...) {
  vec_cast(vctrs::vec_data(x), to) 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_cast.character.retroharmonize_labelled_spss_survey <- function(
    x, 
    to, 
    ...) {
  if (is.character(x)) {
    vec_cast(vctrs::vec_data(x), to, ...)
  } else {
    stop_incompatible_cast(x, to, ...)
  }
}



## Prototype --------------------------------------
#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey <- function(
  x, 
  y, 
  ..., 
  x_arg = "", 
  y_arg = ""
) {
  # data_type <- vctrs::vec_ptype2(vec_data(x), vec_data(y), ..., x_arg = x_arg, y_arg = y_arg)
  data_type <- vec_ptype2(vec_data(x), 
                          vec_data(y), 
                          x_arg = x_arg, 
                          y_arg = y_arg)

  x_labels <- labelled::val_labels(x)
  y_labels <- labelled::val_labels(y)

  dots <- list2(...)

  if (is.null(dots)) {
    dots <- list(orig_name = "")

    same_in <- paste0(
      attr(x, "id"), " and ",
      attr(y, "id"), "."
    )
  } else {
    same_in <- paste0(
      attr(x, "id"), "$", dots$orig_name, " and ",
      attr(y, "id"), "$", dots$orig_name
    )
  }

  if (length(x_labels) == 0 | length(y_labels) == 0) {
    stop_incompatible_type(
      x, y,
      x_arg = dots$orig_name,
      y_arg = paste(names(y_labels), collapse = ", "),
      details = paste0(
        "Must be labelled in ", dots$orig_name
      )
    )
  }

  x_var_label <- labelled::var_label(x)
  y_var_label <- labelled::var_label(y)

  if (!setequal(x_labels, y_labels)) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(x_labels), collapse = ", "),
      y_arg = paste(names(y_labels), collapse = ", "),
      details = paste0(
        "The labelled numeric values must be the same in ", same_in
      )
    )
  }


  if (!setequal(names(x_labels), names(y_labels))) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(x_labels), collapse = ", "),
      y_arg = paste(names(y_labels), collapse = ", "),
      details = paste0(
        "The labels must be the same in ", same_in
      )
    )
  }

  if (!setequal(attr(x, "na_values"), attr(y, "na_values"))) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(attr(x, "na_values")), collapse = ", "),
      y_arg = paste(names(attr(y, "na_values")), collapse = ", "),
      message = paste0(
        "The na_values attributes are not the same in ",
        same_in
      )
    )
  }

  if (!setequal(attr(x, "na_range"), attr(y, "na_range"))) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(attr(x, "na_range")), collapse = ", "),
      y_arg = paste(names(attr(y, "na_range")), collapse = ", "),
      message = paste0(
        "The na_range attributes are not the same in ", same_in
      )
    )
  }

  x_labels <- vec_cast_named(attr(x, "labels"), data_type, x_arg = x_arg)
  y_labels <- vec_cast_named(attr(y, "labels"), data_type, x_arg = y_arg)

  x_label <- attr(x, "label")
  y_label <- attr(y, "label")

  x_id <- attr(x, "id")
  y_id <- attr(y, "id")

  x_attr_names <- names(attributes(x))
  y_attr_names <- names(attributes(y))

  x_orig_attr <- x_attr_names[which(x_attr_names == paste0(x_id, "_name"))]
  y_orig_attr <- y_attr_names[which(y_attr_names == paste0(y_id, "_name"))]

  x_orig_name <- as.character(attr(x, x_orig_attr[1]))
  y_orig_name <- as.character(attr(y, y_orig_attr[1]))

  name_orig <- paste(vec_c(x_orig_name, y_orig_name), collapse = ", ")

  x_na_values <- attr(x, "na_values")
  y_na_values <- attr(y, "na_values")

  x_na_range <- attr(x, "na_range")
  y_na_range <- attr(y, "na_range")

  label <- x_label

  if (!identical(x_label, y_label)) {
    # strip labels if not compatible
    if (is.null(x_label)) {
      label <- y_label
    }
  }

  id <- "multi-wave"

  s1 <- attributes(x)
  s2 <- attributes(y)
  same_attributes <- intersect(names(s1), names(s2))
  compare_attributes <- vapply(same_attributes, function(x) setequal(s1[[x]], s2[[x]]), logical(1))
  matching_arguments <- names(compare_attributes[compare_attributes == TRUE])

  c_vector <- new_labelled_spss_survey(
    x = vec_c(vec_data(x), vec_data(y)),
    labels = x_labels,
    label = label,
    id = id,
    na_values = x_na_values,
    na_range = x_na_range,
    name_orig = name_orig
  )

  for (x_attr in setdiff(names(s1), names(s2))) {
    # Copy the history of x to the new vector
    attr(c_vector, x_attr) <- attr(x, x_attr)
  }

  for (y_attr in setdiff(names(s2), names(s1))) {
    # Copy the history of y to the new vector
    attr(c_vector, y_attr) <- attr(y, y_attr)
  }

  c_vector
}

#' @importFrom haven is_tagged_na
#' @export
vec_cast.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey <- function(
  x, 
  to, ..., 
  x_arg = "", to_arg = ""
) {
  # out_data <- vec_cast(vec_data(x), vec_data(to), ..., x_arg = x_arg, to_arg = to_arg)
  out_data <- vec_cast(vec_data(x), vec_data(to), x_arg = x_arg, to_arg = to_arg)
  x_labels <- labelled::val_labels(x)
  to_labels <- labelled::val_labels(to)

  x_label <- attr(x, "label")
  to_label <- attr(to, "label")

  x_id <- attr(x, "id")
  to_id <- attr(to, "id")

  x_attr_names <- names(attributes(x))
  to_attr_names <- names(attributes(to))

  x_attr_names
  to_attr_names

  x_orig_attr <- x_attr_names[which(x_attr_names == paste0(x_id, "_name"))]
  to_orig_attr <- to_attr_names[which(to_attr_names == paste0(to_id, "_name"))]

  x_orig_name <- as.character(attr(x, x_orig_attr[1]))
  to_orig_name <- as.character(attr(to, to_orig_attr[1]))

  name_orig <- paste(vec_c(x_orig_name, to_orig_name), collapse = ", ")

  x_na_values <- attr(x, "na_values")
  to_na_values <- attr(to, "na_values")

  x_na_range <- attr(x, "na_range")
  to_na_range <- attr(to, "na_range")

  label <- x_label

  if (!identical(x_label, to_label)) {
    # strip labels if not compatible
    if (is.null(x_label)) {
      label <- to_label
    }
  }

  id <- paste(vec_c(x_id, to_id), collapse = ", ")

  out <- new_labelled_spss_survey(
    out_data,
    labels = x_labels,
    label = label,
    id = id,
    na_values = x_na_values,
    na_range = x_na_range,
    name_orig = name_orig
  )

  # do we lose tagged na values? from haven
  if (is.double(x) && !is.double(out)) {
    lossy <- haven::is_tagged_na(x)
    maybe_lossy_cast(out, x, to, lossy,
      x_arg = x_arg,
      to_arg = to_arg,
      details = "Only doubles can hold tagged na values."
    )
  }

  out
}

#' @importFrom cli cat_line
#' @keywords internal
print_attributes <- function(x, full = TRUE) {
  na_values <- attr(x, "na_values")
  if (!is.null(na_values)) {
    cli::cat_line("Missing values: ", 
                     paste(na_values, collapse = ", "))
  }
  na_range <- attr(x, "na_range")
  if (!is.null(na_range)) {
    cli::cat_line("Missing range:  [", 
                     paste(na_range, collapse = ", "), "]")
  }

  if (full == FALSE) invisible(x)
  ## full printing goes on below ---------------------

  history_attributes <- names(attributes(x))
  history_attributes <- history_attributes[
    !history_attributes %in% c(
      "label", "labels",
      "na_values", "na_range", "class", "id"
    )
  ]

  if (length(history_attributes) > 0) {
    last_attribute <- history_attributes[length(history_attributes)]
    history_attributes <- c(history_attributes[1:3], "...", last_attribute)
    history_attributes <- paste(history_attributes, collapse = ", ")
    history_attributes <- gsub("\\,\\s\\.\\.\\.", " [...]", history_attributes)
  }

  cli::cat_line(paste0(
    "See all attributes ",
    history_attributes,
    " with attributes(",
    deparse(substitute(x)),
    ")"
  ))
}

#' @export
#' @importFrom cli cat_line
vec_ptype_full.retroharmonize_labelled_spss_survey <- function(x, ...) {
  paste0("labelled_spss_survey<", 
         vctrs::vec_ptype_full(vctrs::vec_data(x)), ">")
  cli::cat_line("Survey ID: ", attr(x, "id"))
}

#' @export
vec_ptype_abbr.retroharmonize_labelled_spss_survey <- function(x, ...) {
  if (vec_ptype_full(vec_data(x)) == "character") {
    "retroh_chr"
  } else if (vec_ptype_full(vctrs::vec_data(x)) == "integer") {
    "retroh_int"
  } else if (vec_ptype_full(vctrs::vec_data(x)) == "double") {
    "retroh_dbl"
  } else {
    "retroh"
  }
}

#' @export
#' @importFrom cli cat_line
obj_print_header.retroharmonize_labelled_spss_survey <- function(x, ...) {
  cli::cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x))
  invisible(x)
}

#' @export
obj_print_footer.retroharmonize_labelled_spss_survey <- function(x, ...) {
  print_attributes(x)
  invisible(x)
}

#' @rdname labelled_spss_survey
#' @importFrom utils head
#' @importFrom cli cat_line
#' @export
print.retroharmonize_labelled_spss_survey <- function(x, ...) {
  cli::cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x))
  cat(head(vec_data(x), 20))
  cat("\n")
  print_attributes(x)
  invisible(x)
}

## Usual methods  ------------------------------------------------

#' @rdname labelled_spss_survey
#' @export
summary.retroharmonize_labelled_spss_survey <- function(object, ...) {
  if (!is.null(attr(object, "label"))) {
    cat(attr(object, "label"))
  }
  cat("\nNumeric values without coding:\n")
  print(summary(vec_data(object), ...))
  cat("Numeric representation:\n")
  print(summary(as_numeric(object)))
  cat("Factor representation:\n")
  summary(as_factor(object))
}

## Missingness --------------------------------------

#' @rdname labelled_spss_survey
#' @export
is.na.retroharmonize_labelled_spss_survey <- function(x) {
  miss <- NextMethod()
  val <- vctrs::vec_data(x)

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

#' @rdname labelled_spss_survey
#' @exportS3Method
levels.retroharmonize_labelled_spss_survey <- function(x) {
  NULL
}

#' @rdname labelled_spss_survey
#' @export
"names<-.retroharmonize_labelled_spss_survey" <- function(x, value) {
  attr(x, "names") <- value
  x
}

#' @rdname labelled_spss_survey
#' @importFrom haven format_tagged_na
#' @param digits Number of digits to use in string representation in 
#' the format method.
#' @export
format.retroharmonize_labelled_spss_survey <- function(x, 
                                                       ..., 
                                                       digits = getOption("digits")) {
  if (is.double(x)) {
    haven::format_tagged_na(x, digits = digits)
  } else {
    format(vctrs::vec_data(x), ...)
  }
}

# Type system -------------------------------------------------------------

# Import to avoid R CMD check NOTE
#' @importFrom methods setOldClass
# setOldClass(c("retroharmonize_labelled_spss_survey",
#              "haven_labelled_spss",
#              "haven_labelled", "vctrs_vctr"))


#' @rdname labelled_spss_survey
#' @export
is.labelled_spss_survey <- function(x) {
  inherits(x, "retroharmonize_labelled_spss_survey")
}

#' vctrs type and casting methods for labelled survey vectors
#'
#' These methods define how
#' \code{retroharmonize_labelled_spss_survey} objects interact with
#' base vectors and with each other in vctrs-based operations such as
#' concatenation, binding, and coercion.
#'
#' They ensure that labelled survey vectors:
#' \itemize{
#'   \item combine safely with numeric vectors,
#'   \item cast consistently to base types,
#'   \item error on incompatible coercions.
#' }
#'
#' These functions are part of the internal type system and are not
#' intended to be called directly by users.
#'
#' @name labelled_spss_survey_vctrs
#' @keywords internal
NULL

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.retroharmonize_labelled_spss_survey.double <- function(x, 
                                                                  y, 
                                                                  ...) {
  double() 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.double.retroharmonize_labelled_spss_survey <- function(x,
                                                                  y, 
                                                                  ...) {
  double() 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.integer.retroharmonize_labelled_spss_survey <- function(x, 
                                                                   y, 
                                                                   ...) {
  double() 
  }

vec_ptype2.retroharmonize_labelled_spss_survey.integer <- function(x, 
                                                                   y, 
                                                                   ...) {
  double() 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_cast.double.retroharmonize_labelled_spss_survey <- function(x, 
                                                                to, 
                                                                ...) {
  vec_cast(vctrs::vec_data(x), to) 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_cast.integer.retroharmonize_labelled_spss_survey <- function(x,
                                                                 to, 
                                                                 ...) {
  vec_cast(vctrs::vec_data(x), to) 
  }

#' @rdname labelled_spss_survey_vctrs
#' @export
vec_cast.character.retroharmonize_labelled_spss_survey <- function(
    x, 
    to, 
    ...) {
  if (is.character(x)) {
    vec_cast(vctrs::vec_data(x), to, ...)
  } else {
    stop_incompatible_cast(x, to, ...)
  }
}



## Prototype --------------------------------------
#' @rdname labelled_spss_survey_vctrs
#' @export
vec_ptype2.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey <- function(
  x, 
  y, 
  ..., 
  x_arg = "", 
  y_arg = ""
) {
  # data_type <- vctrs::vec_ptype2(vec_data(x), vec_data(y), ..., x_arg = x_arg, y_arg = y_arg)
  data_type <- vec_ptype2(vec_data(x), 
                          vec_data(y), 
                          x_arg = x_arg, 
                          y_arg = y_arg)

  x_labels <- labelled::val_labels(x)
  y_labels <- labelled::val_labels(y)

  dots <- list2(...)

  if (is.null(dots)) {
    dots <- list(orig_name = "")

    same_in <- paste0(
      attr(x, "id"), " and ",
      attr(y, "id"), "."
    )
  } else {
    same_in <- paste0(
      attr(x, "id"), "$", dots$orig_name, " and ",
      attr(y, "id"), "$", dots$orig_name
    )
  }

  if (length(x_labels) == 0 | length(y_labels) == 0) {
    stop_incompatible_type(
      x, y,
      x_arg = dots$orig_name,
      y_arg = paste(names(y_labels), collapse = ", "),
      details = paste0(
        "Must be labelled in ", dots$orig_name
      )
    )
  }

  x_var_label <- labelled::var_label(x)
  y_var_label <- labelled::var_label(y)

  if (!setequal(x_labels, y_labels)) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(x_labels), collapse = ", "),
      y_arg = paste(names(y_labels), collapse = ", "),
      details = paste0(
        "The labelled numeric values must be the same in ", same_in
      )
    )
  }


  if (!setequal(names(x_labels), names(y_labels))) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(x_labels), collapse = ", "),
      y_arg = paste(names(y_labels), collapse = ", "),
      details = paste0(
        "The labels must be the same in ", same_in
      )
    )
  }

  if (!setequal(attr(x, "na_values"), attr(y, "na_values"))) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(attr(x, "na_values")), collapse = ", "),
      y_arg = paste(names(attr(y, "na_values")), collapse = ", "),
      message = paste0(
        "The na_values attributes are not the same in ",
        same_in
      )
    )
  }

  if (!setequal(attr(x, "na_range"), attr(y, "na_range"))) {
    stop_incompatible_type(
      x, y,
      x_arg = paste(names(attr(x, "na_range")), collapse = ", "),
      y_arg = paste(names(attr(y, "na_range")), collapse = ", "),
      message = paste0(
        "The na_range attributes are not the same in ", same_in
      )
    )
  }

  x_labels <- vec_cast_named(attr(x, "labels"), data_type, x_arg = x_arg)
  y_labels <- vec_cast_named(attr(y, "labels"), data_type, x_arg = y_arg)

  x_label <- attr(x, "label")
  y_label <- attr(y, "label")

  x_id <- attr(x, "id")
  y_id <- attr(y, "id")

  x_attr_names <- names(attributes(x))
  y_attr_names <- names(attributes(y))

  x_orig_attr <- x_attr_names[which(x_attr_names == paste0(x_id, "_name"))]
  y_orig_attr <- y_attr_names[which(y_attr_names == paste0(y_id, "_name"))]

  x_orig_name <- as.character(attr(x, x_orig_attr[1]))
  y_orig_name <- as.character(attr(y, y_orig_attr[1]))

  name_orig <- paste(vec_c(x_orig_name, y_orig_name), collapse = ", ")

  x_na_values <- attr(x, "na_values")
  y_na_values <- attr(y, "na_values")

  x_na_range <- attr(x, "na_range")
  y_na_range <- attr(y, "na_range")

  label <- x_label

  if (!identical(x_label, y_label)) {
    # strip labels if not compatible
    if (is.null(x_label)) {
      label <- y_label
    }
  }

  id <- "multi-wave"

  s1 <- attributes(x)
  s2 <- attributes(y)
  same_attributes <- intersect(names(s1), names(s2))
  compare_attributes <- vapply(same_attributes, function(x) setequal(s1[[x]], s2[[x]]), logical(1))
  matching_arguments <- names(compare_attributes[compare_attributes == TRUE])

  c_vector <- new_labelled_spss_survey(
    x = vec_c(vec_data(x), vec_data(y)),
    labels = x_labels,
    label = label,
    id = id,
    na_values = x_na_values,
    na_range = x_na_range,
    name_orig = name_orig
  )

  for (x_attr in setdiff(names(s1), names(s2))) {
    # Copy the history of x to the new vector
    attr(c_vector, x_attr) <- attr(x, x_attr)
  }

  for (y_attr in setdiff(names(s2), names(s1))) {
    # Copy the history of y to the new vector
    attr(c_vector, y_attr) <- attr(y, y_attr)
  }

  c_vector
}

#' @importFrom haven is_tagged_na
#' @export
vec_cast.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey <- function(
  x, 
  to, ..., 
  x_arg = "", to_arg = ""
) {
  # out_data <- vec_cast(vec_data(x), vec_data(to), ..., x_arg = x_arg, to_arg = to_arg)
  out_data <- vec_cast(vec_data(x), vec_data(to), x_arg = x_arg, to_arg = to_arg)
  x_labels <- labelled::val_labels(x)
  to_labels <- labelled::val_labels(to)

  x_label <- attr(x, "label")
  to_label <- attr(to, "label")

  x_id <- attr(x, "id")
  to_id <- attr(to, "id")

  x_attr_names <- names(attributes(x))
  to_attr_names <- names(attributes(to))

  x_attr_names
  to_attr_names

  x_orig_attr <- x_attr_names[which(x_attr_names == paste0(x_id, "_name"))]
  to_orig_attr <- to_attr_names[which(to_attr_names == paste0(to_id, "_name"))]

  x_orig_name <- as.character(attr(x, x_orig_attr[1]))
  to_orig_name <- as.character(attr(to, to_orig_attr[1]))

  name_orig <- paste(vec_c(x_orig_name, to_orig_name), collapse = ", ")

  x_na_values <- attr(x, "na_values")
  to_na_values <- attr(to, "na_values")

  x_na_range <- attr(x, "na_range")
  to_na_range <- attr(to, "na_range")

  label <- x_label

  if (!identical(x_label, to_label)) {
    # strip labels if not compatible
    if (is.null(x_label)) {
      label <- to_label
    }
  }

  id <- paste(vec_c(x_id, to_id), collapse = ", ")

  out <- new_labelled_spss_survey(
    out_data,
    labels = x_labels,
    label = label,
    id = id,
    na_values = x_na_values,
    na_range = x_na_range,
    name_orig = name_orig
  )

  # do we lose tagged na values? from haven
  if (is.double(x) && !is.double(out)) {
    lossy <- haven::is_tagged_na(x)
    maybe_lossy_cast(out, x, to, lossy,
      x_arg = x_arg,
      to_arg = to_arg,
      details = "Only doubles can hold tagged na values."
    )
  }

  out
}
