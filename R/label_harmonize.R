#' Normalize value and variable labels 
#'
#' \code{label_normalize} removes special characters, whitespace, 
#' and other typical typing errors.
#' 
#' \code{var_label_normalize} changes the vector to snake_case.
#' \code{val_label_normalize} removes possible chunks from question
#' identifiers.
#' 
#' The functions \code{var_label_normalize} and 
#' \code{val_label_normalize} may 
#' be differently implemented for various survey series.
#' 
#' @param x A character vector of labels to be normalized.
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
#' @export

label_normalize <- function(x) {
  ## unit tests for this function are in
  ## tests/testthat/test-label_normalize.R
  ## please add test after correcting for unexpected results
  
  y <- gsub("\\s+", " ", x)
  y <- gsub( "don\\'t", "do not", y)
  y <- gsub( "Don\\'t", "Do not", y)
  y <- gsub( '\\&', '_and_', y)
  y <- gsub( '\\+', '_plus_', y)
  y <- gsub( '\\%', '_pct_', y)
  y <- gsub( '>=', '_ge_', y)
  y <- gsub( '<=', '_le_', y)
  y <- gsub( '<', '_lt_', y)
  y <- gsub( '>', '_gt_', y)
  y <- gsub('\\.|-|\\:|\\;|\\/|\\(|\\)|\\!', '_', y)
  y  <- gsub(tolower("15_plus"), "gt 15", y)
  y <- gsub( '__|___|___|\\s_|_\\s', '_', y )
  y <- gsub( '^_', '', y )
  y <- gsub( '_$', '', y )
  y
}

#' @rdname label_normalize 
#' @importFrom snakecase to_snake_case
#' @export
var_label_normalize <- function(x) {
  x <- trimws(as.character(x), which = "both")
  snakecase::to_sentence_case(label_normalize(x))
}

#' @rdname label_normalize 
#' @importFrom snakecase to_snake_case
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


