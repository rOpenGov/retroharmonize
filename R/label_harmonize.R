#' Normalize value and variable labels 
#'
#' \code{label_normalize} removes special characters, whitespace, 
#' and other typical typing errors.
#' 
#' \code{var_label_normalize} and \code{val_label_normalize} removes possible 
#' chunks from question identifiers.
#' 
#' The functions \code{var_label_normalize} and 
#' \code{val_label_normalize} may 
#' be differently implemented for various survey series.
#' 
#' @param x A character vector of labels to be normalized.
#' @importFrom snakecase to_snake_case
#' @examples 
#' label_normalize (
#' c("Don't know", " TRUST", "DO NOT  TRUST", 
#'   "inap in Q.3", "Not 100%", "TRUST < 50%", 
#'   "TRUST >=90%", "Verify & Check", "TRUST 99%+"))
#'  
#'  var_label_normalize ( 
#'       c("Q1_Do you trust the national government?", 
#'         " Do you trust the European Commission")
#'         )
#'
#'   val_label_normalize ( 
#'       c("Q1_Do you trust the national government?", 
#'         " Do you trust the European Commission")
#'         )
#' @family variable label harmonization functions
#' @return Returns a suggested, normalized label without special characters. The 
#' \code{var_label_normalize} and \code{val_label_normalize} returns them in 
#' \code{snake_case} for programmatic use.
#' @export

label_normalize <- function(x) {
  ## unit tests for this function are in
  ## tests/testthat/test-label_normalize.R
  ## please add test after correcting for unexpected results
  
  y <- gsub("\\s+", " ", x)
  y <- gsub( "on\\'t", "o not", y) # don't|Don't
  y <- gsub( "ON\\'T", "O NOT", y)
  y <- gsub( "an\\'t", "an not", y) # can't|Can't
  y <- gsub( "AN\\'T", "AN NOT", y)
  y <- gsub( "\\'ll", "will", y)  # you'll, he'll (can be confused with shall)
  y <- gsub( "\\'LL", "WILL", y)
  y <- gsub( '\\&', ' and ', y)
  y <- gsub( '\\+', ' plus ', y)
  y <- gsub( '\\%', ' pct ', y)
  y <- gsub( '>=', ' ge ', y)
  y <- gsub( '<=', ' le ', y)
  y <- gsub( '<', ' lt ', y)
  y <- gsub( '>', '_gt_', y)
  y <- gsub('\\.|-|\\:|\\;|\\/|\\(|\\)|\\!|\\?', ' ', y)
  y  <- gsub(tolower("15_plus"), "gt 15", y)
  y <- gsub( '__|___|___|\\s_|_\\s', '_', y )
  y <- gsub("\\s+", " ", y)
  y <- gsub( '^_', '', y )
  y <- gsub( '_$', '', y )
  trimws(tolower(y), which = "both")
}

#' @rdname label_normalize 
#' @importFrom snakecase to_snake_case
#' @family harmonization functions
#' @export
var_label_normalize <- function(x) {
  y <- tolower (as.character(x))
  y <- gsub( '^q[abcde]\\d{1,2}_\\d{1,2}', '', y )  # remove QA117_1
  y <- gsub( '^q[abcde]\\d{1,2}', '', y )  # remove QA1, QB25 etc
  y <- gsub( '^d\\d{1,2}', '', y )       # removed d26_ etc
  y <- gsub( '^c\\d{1,2}', '', y )       # removed c26_ etc
  y <- gsub ( "^p\\d+{1,}_", "", y)  #remove p6_ like starts
  y <- gsub ( "^p\\d+{1,}\\s", "", y)  #remove p6  like starts
  y <- gsub ( "^q\\d+{1,}_", "", y)  #remove q1_ like starts
  y <- gsub ( "^q\\d+{1,}[abcdefghijkl]", "", y)  #remove q1  like starts
  y <- gsub( '^\\d{1,2}_', '', y ) #remove leading number 1_
  
  y <- label_normalize(y)
  y <- gsub(' +',' ',y) 
  y <- trimws(tolower(as.character(y)), which = "both")
  snakecase::to_snake_case(y)
}

#' @rdname label_normalize 
#' @importFrom snakecase to_snake_case
#' @family harmonization functions
#' @export
val_label_normalize <- function(x) {
  
  y <- trimws(tolower(as.character(x)), which = "both")
  y <- label_normalize(x)
  y <- gsub( '^q[abcde]\\d{1,2}_\\d{1,2}', '', y )  # remove QA117_1
  y <- gsub( '^q[abcde]\\d{1,2}', '', y )  # remove QA1, QB25 etc
  y <- gsub( '^d\\d{1,2}', '', y )       # removed d26_ etc
  y <- gsub( '^c\\d{1,2}', '', y )       # removed c26_ etc
  y <- gsub ( "^p\\d+{1,}_", "", y)  #remove p6_ like starts
  y <- gsub ( "^p\\d+{1,}\\s", "", y)  #remove p6  like starts
  y <- gsub ( "^q\\d+{1,}_", "", y)  #remove q1_ like starts
  y <- gsub ( "^q\\d+{1,}\\s", "", y)  #remove q1  like starts
  y <- gsub( '^\\d{1,2}_', '', y ) #remove leading number 1_

  y <- trimws(tolower(as.character(y)), which = "both")
  y <- gsub(' +',' ',y) 
  snakecase::to_snake_case(y)
}