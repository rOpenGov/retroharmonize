#' @title Harmonize values in surveys
#'
#' @description
#' Harmonize value codes and value labels across multiple surveys and
#' combine them into a single data frame.
#'
#' @details
#' The function first aligns the structure of all surveys by ensuring that
#' they contain the same set of variables. Missing variables are added and
#' filled with appropriate missing values depending on their type.
#'
#' Variables of class \code{"retroharmonize_labelled_spss_survey"} are then
#' harmonized by applying a user-supplied function \code{.f} to each variable
#' separately within each survey.
#'
#' The harmonization function \code{.f} must return a vector of the same length
#' as its input. If \code{.f} returns \code{NULL}, the original variable is kept
#' unchanged.
#'
#' Prior to version 0.2.0 this function was called \code{harmonize_waves}.
#'
#' @param survey_list
#' A list of surveys (data frames). In earlier versions this argument was called
#' \code{waves}.
#'
#' @param .f
#' A function applied to each labelled variable
#' (class \code{"retroharmonize_labelled_spss_survey"}). The function must not
#' change the length of the input vector.
#'
#' @param status_message
#' Logical. If \code{TRUE}, prints the identifier of each survey as it is
#' processed.
#'
#' @return
#' A data frame containing the row-wise combination of all surveys, with
#' harmonized labelled variables and preserved attributes describing the
#' original surveys.
#'
#' @family harmonization functions
#'
#' @export
#'
#' @importFrom dplyr select bind_cols mutate_all pull
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @importFrom rlang set_names
#' @importFrom haven labelled_spss
#'
#' @examples
#' \donttest{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_files <- dir(examples_dir, pattern = "\\.rds$", full.names = TRUE)
#'
#' surveys <- read_surveys(
#'   survey_files,
#'   export_path = NULL
#' )
#'
#' # Keep only supported variable types
#' surveys <- lapply(
#'   surveys,
#'   function(s) {
#'     s[, vapply(
#'       s,
#'       function(x) inherits(x, c(
#'         "retroharmonize_labelled_spss_survey",
#'         "numeric",
#'         "character",
#'         "Date"
#'       )),
#'       logical(1)
#'     )]
#'   }
#' )
#'
#' # Identity harmonization (no-op)
#' harmonized <- harmonize_survey_values(
#'   survey_list = surveys,
#'   .f = function(x) x,
#'   status_message = FALSE
#' )
#'
#' head(harmonized)
#' }


harmonize_survey_values <- function(
    survey_list,
    .f,
    status_message = FALSE
) {
  
  validate_survey_list(survey_list)
  
  all_names <- unique(unlist(lapply(survey_list, names)))
  
  ## classify variables robustly
  classes <- unlist(lapply(
    survey_list,
    function(x) lapply(x, function(y) {
      if (inherits(y, "retroharmonize_labelled_spss_survey")) {
        "retroharmonize_labelled_spss_survey"
      } else if (inherits(y, c("numeric", "double", "integer"))) {
        "numeric"
      } else if (inherits(y, "character")) {
        "character"
      } else if (inherits(y, "Date")) {
        "Date"
      } else {
        "other"
      }
    })
  ))
  
  retroharmonized <- unique(names(classes)[classes == "retroharmonize_labelled_spss_survey"])
  numerics <- unique(names(classes)[classes == "numeric"])
  characters <- unique(names(classes)[classes == "character"])
  dates <- unique(names(classes)[classes == "Date"])
  other_types <- names(classes)[classes == "other"]
  
  if (length(other_types) > 0) {
    stop(
      "Only labelled_spss_survey, numeric, character and Date types are allowed",
      call. = FALSE
    )
  }
  
  
  original_attributes <- document_surveys(survey_list)
  
  ## ---- extend surveys so all have same columns ----
  extend_survey <- function(dat) {
    
    to_add_rh <- setdiff(retroharmonized, names(dat))
    to_add_num <- setdiff(numerics, names(dat))
    to_add_chr <- setdiff(characters, names(dat))
    to_add_date <- setdiff(dates, names(dat))
    
    out <- dat
    
    if (length(to_add_num) > 0) {
      out <- dplyr::bind_cols(
        out,
        as.data.frame(matrix(NA_real_, nrow(dat), length(to_add_num)),
                      stringsAsFactors = FALSE) %>%
          rlang::set_names(to_add_num)
      )
    }
    
    if (length(to_add_chr) > 0) {
      out <- dplyr::bind_cols(
        out,
        as.data.frame(matrix(NA_character_, nrow(dat), length(to_add_chr)),
                      stringsAsFactors = FALSE) %>%
          rlang::set_names(to_add_chr)
      )
    }
    
    if (length(to_add_date) > 0) {
      out <- dplyr::bind_cols(
        out,
        as.data.frame(matrix(as.Date(NA), nrow(dat), length(to_add_date)),
                      stringsAsFactors = FALSE) %>%
          rlang::set_names(to_add_date)
      )
    }
    
    if (length(to_add_rh) > 0) {
      
      add_rh_df <- as.data.frame(
        matrix(99999, nrow(dat), length(to_add_rh)),
        stringsAsFactors = FALSE
      ) %>%
        rlang::set_names(to_add_rh)
      
      add_rh_df <- dplyr::mutate_all(add_rh_df, function(x) {
        haven::labelled_spss(
          x,
          labels = c(inap = 99999),
          na_values = c(
            "do_not_know" = 99997,
            "declined" = 99998,
            "inap" = 99999
          )
        )
      })
      
      for (i in seq_len(ncol(add_rh_df))) {
        attr(add_rh_df[, i], "label") <- to_add_rh[i]
      }
      
      add_rh_df <- tibble::as_tibble(
        lapply(add_rh_df, function(x) {
          as_labelled_spss_survey(x, attr(dat, "id"))
        })
      )
      
      out <- dplyr::bind_cols(out, add_rh_df)
    }
    
    dplyr::select(out, tidyselect::all_of(all_names))
  }
  
  extended <- lapply(survey_list, extend_survey)
  
  ## ---- harmonize labelled variables ----
  to_harmonize <- lapply(
    extended,
    function(x) dplyr::select(x, tidyselect::all_of(retroharmonized))
  )
  
  fn_harmonize <- function(dat) {
    
    harmonized <- lapply(dat, function(x) {
      out <- .f(x)
      
      if (is.null(out)) {
        return(x)
      }
      
      if (length(out) != length(x)) {
        stop(
          "Harmonization function must not change vector length",
          call. = FALSE
        )
      }
      
      out
    })
    
    tibble::as_tibble(harmonized)
  }
  
  harmonized <- lapply(to_harmonize, fn_harmonize)
  
  ## ---- bind everything together ----
  result <- harmonized[[1]]
  for (i in 2:length(harmonized)) {
    result <- vctrs::vec_rbind(result, harmonized[[i]])
  }
  
  attr(result, "id") <- paste("Surveys:", paste(original_attributes$id, collapse = "; "))
  attr(result, "filename") <- paste("Original files:", paste(original_attributes$filename, collapse = "; "))
  
  result
}



#' @rdname harmonize_survey_values
#' @details The earlier form \code{harmonize_waves} is deprecated.
#' The function is currently called \code{\link{harmonize_waves}}.
#' @param waves A list of surveys. Deprecated.
#' @export


harmonize_waves <- function(waves, .f, status_message = FALSE) {
  .Deprecated("harmonize_waves ",
    msg = "harmonize_waves() is deprecated, use harmonize_survey_values() instead",
    old = "harmonize_waves"
  )
  harmonize_survey_values(
    survey_list = waves,
    .f = .f,
    status_message = status_message
  )
}



