
#' @export
vec_ptype2.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey  <- function(x, y, ..., x_arg = "", y_arg = "") {
  data_type <- vec_ptype2(vec_data(x), vec_data(y), ..., x_arg = x_arg, y_arg = y_arg)
  #data_type <- vec_ptype2(vec_data(x), vec_data(y), x_arg = x_arg, y_arg = y_arg)
  
  x_labels <- vec_cast_named(attr(x, "labels"), data_type, x_arg = x_arg)
  y_labels <- vec_cast_named(attr(y, "labels"), data_type, x_arg = y_arg)
  
  x_label <- attr(x, "label")
  y_label <- attr(y, "label")
  
  x_id <- attr(x, "id") 
  y_id <- attr(y, "id")
  
  x_attr_names <- names(attributes(x))
  to_attr_names <- names(attributes(to))
  
  x_orig_attr <- x_attr_names [which (x_attr_names ==  paste0(x_id, "_name" ))]
  #to_orig_attr <- to_attr_names[which (to_attr_names ==  paste0(to_id, "_name" ))]
  
  x_orig_name <- as.character(attr(x, x_orig_attr[1]))
  #to_orig_name <- as.character(attr(to, to_orig_attr[1]))
  y_orig_name  <- y_orig_name
  
  name_orig <- vec_c(x_orig_name, y_orig_name)

  if (!identical(x_labels, y_labels)) {
    # strip labels if not compatible
    stop("Labels must be identifical for combining labelled_spss_survey")
  }
  
  x_na_values <- attr(x, "na_values")
  y_na_values <- attr(y, "na_values")
  
  if (!identical(x_na_values, y_na_values)) {
    # strip labels if not compatible
    stop("User-defined missing values must be identifical for combining labelled_spss_survey")
  }
  
  x_na_range <- attr(x, "na_range")
  y_na_range <- attr(y, "na_range")
  
  if (!identical(x_na_range,y_na_range)) {
    # strip labels if not compatible
    stop("User-defined missing ranges must be identifical for combining labelled_spss_survey")
    
  }
  
  label <- x_label
  
  if (!identical(x_label, y_label)) {
    # strip labels if not compatible
    if (is.null(x_label)) {
      label <- y_label
    }
  }
  
  id <- vec_c(x_id, y_id)
  
  s1 <- attributes (x)
  s2 <- attributes (y)
  same_attributes <- intersect(names(s1), names(s2))
  compare_attributes <- vapply ( same_attributes, function(x) setequal(s1[[x]],s2[[x]]), logical(1))
  matching_arguments <- names(compare_attributes[ compare_attributes == TRUE ]) 
  
  c_vector <- new_labelled_spss_survey(
    data_type, 
    labels = x_labels,
    label = label, 
    id = id, 
    na_values = x_na_values,
    na_range = x_na_range,
    name_orig = name_orig )
  
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

#' @export
vec_cast.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey <- function(x, to, ..., x_arg = "", to_arg = "") {
  out_data <- vec_cast(vec_data(x), vec_data(to), ..., x_arg = x_arg, to_arg = to_arg)
  out_data <- vec_cast(vec_data(x), vec_data(to),  x_arg = x_arg, to_arg = to_arg)
  x_labels <- vec_cast_named(attr(x, "labels"), data_type, x_arg = x_arg)
  to_labels <- vec_cast_named(attr(to, "labels"), data_type, x_arg = y_arg)
  
  x_label  <- attr(x, "label")
  to_label <- attr(to, "label")
  
  x_id  <- attr(x, "id") 
  to_id <- attr(to, "id")
  
  x_attr_names <- names(attributes(x))
  to_attr_names <- names(attributes(to))
  
  x_orig_attr <- x_attr_names [which (x_attr_names ==  paste0(x_id, "_name" ))]
  #to_orig_attr <- to_attr_names[which (to_attr_names ==  paste0(to_id, "_name" ))]

  x_orig_name <- as.character(attr(x, x_orig_attr[1]))
  #to_orig_name <- as.character(attr(to, to_orig_attr[1]))
  
  to_orig_name  <- x_orig_name
  
  name_orig <- vec_c(x_orig_name, to_orig_name)
  
  if (!identical(x_labels, to_labels)) {
    # strip labels if not compatible
    stop("Labels must be identifical for combining labelled_spss_survey")
  }
  
  x_na_values  <- attr(x, "na_values")
  to_na_values <- attr(to, "na_values")
  
  if (!identical(x_na_values, to_na_values)) {
    # strip labels if not compatible
    stop("User-defined missing values must be identifical for combining labelled_spss_survey")
  }
  
  x_na_range  <- attr(x, "na_range")
  to_na_range <- attr(to, "na_range")
  
  if (!identical(x_na_range,to_na_range)) {
    # strip labels if not compatible
    stop("User-defined missing ranges must be identifical for combining labelled_spss_survey")
    
  }
  
  label <- x_label 
  
  if (!identical(x_label, to_label)) {
    # strip labels if not compatible
    if (is.null(x_label)) {
      label <- to_label
    }
  }
  
  id <- vec_c(x_id, to_id)
  
  out <- new_labelled_spss_survey(
    out_data, 
    labels = x_labels,
    label = label, 
    id = id, 
    na_values = x_na_values,
    na_range = x_na_range,
    name_orig = name_orig )
  
  
  # do we lose tagged na values? from haven
  if (is.double(x) && !is.double(out)) {
    lossy <- is_tagged_na(x)
    maybe_lossy_cast(out, x, to, lossy,
                     x_arg = x_arg,
                     to_arg = to_arg,
                     details = "Only doubles can hold tagged na values."
    )
  }
  
  out
}
