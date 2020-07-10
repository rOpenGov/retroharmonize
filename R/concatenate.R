#' @title Concatenate haven_labelled_spss vectors
#'
#' @param x A haven_labelled_spss vector.
#' @param y A haven_labelled_spss vector.
#' @return A concatenated haven_labelled_spss vector. Returns an error
#' if the attributes do not match. Gives a warning when only the variable
#' label do not match.
#' @importFrom labelled labelled val_labels
#' @importFrom haven labelled_spss
#' @examples
#' v1 <- labelled::labelled(
#' c(3,4,4,3,8, 9),
#' c(YES = 3, NO = 4, `WRONG LABEL` = 8, REFUSED = 9)
#' )
#' v2 <- labelled::labelled(
#'   c(4,3,3,9),
#'   c(YES = 3, NO = 4, `WRONG LABEL` = 8, REFUSED = 9)
#' )
#' s1 <- haven::labelled_spss(
#'   x = unclass(v1),         # remove labels from earlier defined
#'   labels = labelled::val_labels(v1), # use the labels from earlier defined
#'   na_values = NULL,
#'   na_range = 8:9,
#'   label = "Variable Example"
#' )
#'
#' s2 <- haven::labelled_spss(
#'   x = unclass(v2),         # remove labels from earlier defined
#'   labels = labelled::val_labels(v2), # use the labels from earlier defined
#'   na_values = NULL,
#'   na_range = 8:9,
#'   label = "Variable Example"
#' )
#' concatenate (s1,s2)
#' @importFrom haven labelled_spss labelled
#' @family joining functions
#' @export

concatenate <- function(x, y) {
  if ( ! all(c(is.labelled(x), is.labelled(y))) ) {
    stop ("Both arguments must be labelled")
  }

  if ( sum(is.labelled_spss(x), is.labelled_spss(y)) == 1 ) {
    if ( ! is.labelled_spss(x) ) {

      x_na_values <- labelled::na_values(y)
      y_labels <- labelled::val_labels(y)

      x <- convert_to_labelled_spss(x, y_labels [ y_labels  %in% x_na_values ])
      }
  }

  if ( is.null(attr(x, "na_range")) ) {
    if ( ! is.null(attr(y, "na_range")) ) {
      stop("The first (x) na_range is NULL, the second (y) is not.")
    }
  } else if ( is.null(attr(y, "na_range")) ) {
    stop("The second (y) na_range is NULL, the first (x) is not.")
  } else {
    if ( any( attr(x, "na_range" ) != attr(y, "na_range" )) ) {
      stop ("The na_range attribute must be equal.")
    }
  }

  if ( is.null(attr(x, "na_values")) ) {
    if ( ! is.null(attr(y, "na_values")) ) {
      stop("The first (x) na_values is NULL, the second (y) is not.")
    }
  } else if ( is.null(attr(y, "na_values")) ) {
    stop("The second (y) na_values is NULL, the first (x) is not.")
  } else {
    if ( any( attr(x, "na_values" ) != attr(y, "na_values" )) ) {
      stop ("The na_values attribute must be equal.")
    }
  }

  if ( ! is.labelled (x) && is.labelled(y) ) {
    stop ("Both x and y must be labelled.")
  }

  if ( ! all(class(x)==class(y)) ) {
    stop ("class(x)=",
          paste(class(x), collapse=","),
          "\nbut class(y)=", paste(class(y), collapse =","))
  }

  label_x <- attr(x, "label")
  label_y <- attr(y, "label")

  if ( sum(is.null(label_x), is.null(label_y))==1 ) {
    if (is.null(label_x)) {
      attr(x, "label") <- attr(y, "label")
      warning ("The variable label of y <", label_y, "> will be used as variable label.")
    } else {
      warning("The variable label of x <", label_x, "> will be used as variable label.")
    }

    all.equal( label_x, label_y )

    label_x
    label_y

  } else {
    if ( ! all.equal( label_x, label_y ) ) {
      warning ("The variable labels are not the same, <", label_x, "> of x will be used.")
    }
  }

  z <- c(unclass(x), unclass(y))
  new_vctr(z,
           label = attr(x, "label"),
           labels = attr(x, "labels"),
           na_range = attr(x, "na_range"),
           na_values = attr(x, "na_values"),
           class = class(x),
           inherit_base_type = TRUE)
}

