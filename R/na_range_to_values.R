#' Harmonize user-defined missing value ranges
#'
#' Harmonize the \code{na_values} attribute with 
#' \code{na_range}, if the latter is present.
#' 
#' \code{na_range_to_values()} tests if the function needs to be 
#' called for \code{na_values} harmonization. The \code{na_range}
#' is often missing and less likely to cause logical problems 
#' when joining survey answers.
#'
#' @importFrom labelled na_range na_values labelled_spss
#' @param x A labelled_spss or labelled_spss_survey vector
#' @return A \code{x} with harmonized \code{na_values} and 
#' \code{na_range} attributes.
#' If \code{min(na_values)} or \code{max(na_values)} than the left- and 
#' right-hand value of \code{na_range}, it gives a warning and adjusts
#' the original \code{na_range}.
#' @examples 
#' var1 <- labelled::labelled_spss(
#'   x = c(1,0,1,1,0,8,9), 
#'   labels = c("TRUST" = 1, 
#'              "NOT TRUST" = 0, 
#'              "DON'T KNOW" = 8, 
#'              "INAP. HERE" = 9), 
#' na_range = c(8,12))
#'   
#' na_range_to_values(var1)
#' as_numeric(na_range_to_values(var1))
#' as_character(na_range_to_values(var1))
#' @family harmonization functions
#' @export

na_range_to_values <- function(x){
  
  if( ! inherits(x, "haven_labelled_spss")) return(x)
  if ( is.null(attr(x, "na_range")) ) return(x)
  
  na_values <- vector (mode = "double", length = 0 )
  
  if ( ! is.null(attr(x, "na_value"))) {
    na_values <- attr(x, "na_value")
  }
  
  na_min <- labelled::na_range(x)[1]
  na_max <- labelled::na_range(x)[2]
  
  # both na_range and na_values present ---------------------
  if ( length(na_values) >0 ) { 
    
    if (min(na_values) < na_min) {
      warning("Inconsistent missing ranges: min(na_values) < min(na_range)")
      na_min <- min(na_values)
    }
    
    if (max(na_values) > na_max) {
      warning("Inconsistent missing ranges: max(na_values) > max(na_range)")
     na_max <- max(na_values)
    }
  } 
  
  ## find the missing values that are actually present in the vector -----------
  above_min <- unclass(x)[(unclass(x) >= na_min)] 
  na_values <- above_min[above_min <= na_max]
  
  ## if none, make the to attributes consistent only -----------------
  if (length(na_values)==0) {
    na_values <- na_min:na_max
  }
  
  labelled::na_values(x) <- na_values
  labelled::na_range(x) <- c(na_min, na_max)
  x
}

#' @rdname na_range_to_values
#' @export
is.na_range_to_values <- function(x) {
  ## test if na_values and na_range must be harmonized
  !is.null(attr(x, "na_values")) && !is.null(attr(x, "na_range")) 
}