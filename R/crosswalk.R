#' Crosswalk and harmonize surveys
#'
#' Harmonize one or more surveys using a crosswalk table that defines how
#' variable names, value labels, numeric codes, and variable classes should
#' be aligned across surveys.
#'
#' A crosswalk table can be created with [crosswalk_table_create()] or supplied
#' manually as a data frame. At a minimum, the table must contain columns
#' `id`, `var_name_orig`, and `var_name_target`. Additional columns enable
#' harmonization of value labels, numeric codes, missing values, and variable
#' classes.
#'
#' @param crosswalk_table A crosswalk table created with
#'   [crosswalk_table_create()] or a data frame containing at least the columns
#'   `id`, `var_name_orig`, and `var_name_target`.
#'
#'   If the columns `val_label_orig` and `val_label_target` are present,
#'   value labels are harmonized. If `val_numeric_orig` and
#'   `val_numeric_target` are present, numeric codes are harmonized.
#'   If `class_target` is present, variables are coerced to the specified
#'   target class (`"factor"`, `"numeric"`, or `"character"`) using
#'   [as_factor()], [as_numeric()], or [as_character()].
#'
#' @param survey_list A list of survey objects to be harmonized.
#'
#' @param survey_paths Optional character vector of file paths to surveys.
#'   Used when surveys must be read from disk before harmonization.
#'
#' @param import_path Optional base directory used to resolve `survey_paths`.
#'   This is primarily intended for workflows where surveys are stored outside
#'   the current working directory.
#'
#' @param na_values Optional named vector defining numeric codes to be treated
#'   as missing values. Names correspond to missing-value labels.
#'
#' @return
#' `crosswalk_surveys()` returns a list of harmonized survey data frames.
#' `crosswalk()` returns either a single data frame (if only one survey is
#' harmonized) or a merged data frame combining all harmonized surveys.
#'
#' @seealso
#' [crosswalk_table_create()] to create a crosswalk table,
#' [harmonize_survey_variables()] for lower-level variable harmonization.
#'
#' @family harmonization functions
#'
#' @examples
#' \dontrun{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_files <- dir(examples_dir, pattern = "\\.rds$")
#'
#' surveys <- read_surveys(
#'   file.path(examples_dir, survey_files),
#'   save_to_rds = FALSE
#' )
#'
#' metadata <- metadata_create(survey_list = surveys)
#'
#' crosswalk_table <- crosswalk_table_create(metadata)
#'
#' harmonized <- crosswalk_surveys(
#'   crosswalk_table = crosswalk_table,
#'   survey_list = surveys
#' )
#' }
#'
#' @export
crosswalk_surveys <- function(crosswalk_table,
                              survey_list = NULL,
                              survey_paths = NULL,
                              import_path = NULL,
                              na_values = NULL) {
  
  # ------------------------------------------------------------------
  # 0. Validate inputs
  # ------------------------------------------------------------------
  is.crosswalk_table(crosswalk_table)
  
  if (is.null(survey_list)) {
    stop("`survey_list` must be provided.", call. = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 1. Keep only surveys referenced in the crosswalk table
  # ------------------------------------------------------------------
  survey_ids <- vapply(survey_list, function(x) attr(x, "id"), character(1))
  keep <- survey_ids %in% unique(crosswalk_table$id)
  survey_list <- survey_list[keep]
  
  if (length(survey_list) == 0) {
    stop("No surveys match IDs found in the crosswalk table.", call. = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 2. Harmonize variable names (structural harmonization)
  # ------------------------------------------------------------------
  harmonized_surveys <- harmonize_survey_variables(
    survey_list   = survey_list,
    survey_paths  = survey_paths,
    import_path   = import_path,
    crosswalk_table = crosswalk_table
  )
  
  # ------------------------------------------------------------------
  # 3. Helper: relabel values within a single survey
  # ------------------------------------------------------------------
  relabel_survey <- function(survey, selection) {
    
    selection <- selection %>%
      dplyr::filter(!is.na(val_label_orig))
    
    vars <- unique(selection$var_name_target)
    if (length(vars) == 0) return(survey)
    
    for (var in vars) {
      
      mapping <- selection %>%
        dplyr::filter(var_name_target == var)
      
      if (!is.numeric(mapping$val_numeric_target)) {
        stop("`val_numeric_target` must be numeric.", call. = FALSE)
      }
      
      # Determine NA values (crosswalk overrides user input)
      local_na_values <- NULL
      if (all(c("na_numeric_target", "na_label_target") %in% names(mapping))) {
        idx <- !is.na(mapping$na_numeric_target)
        if (any(idx)) {
          local_na_values <- mapping$na_numeric_target[idx]
          names(local_na_values) <- mapping$na_label_target[idx]
        }
      } else {
        local_na_values <- na_values
      }
      
      harmonize_one <- function(x) {
        harmonize_values(
          x = x,
          harmonize_labels = list(
            from = paste0("^", mapping$val_label_orig, "$"),
            to   = mapping$val_label_target,
            numeric_values = mapping$val_numeric_target
          ),
          na_values = local_na_values
        )
      }
      
      survey <- survey %>%
        dplyr::mutate(dplyr::across(dplyr::any_of(var), harmonize_one))
    }
    
    survey
  }
  
  # ------------------------------------------------------------------
  # 4. Harmonize a single survey (labels + classes)
  # ------------------------------------------------------------------
  harmonize_one_survey <- function(survey) {
    
    survey_id <- attr(survey, "id")
    if (length(survey_id) == 0) {
      stop("Survey has no `id` attribute.", call. = FALSE)
    }
    
    survey <- survey %>%
      dplyr::mutate(id = survey_id) %>%
      dplyr::relocate(id, .before = dplyr::everything())
    
    selection <- crosswalk_table %>%
      dplyr::filter(id == survey_id)
    
    if (all(c("val_label_orig", "val_label_target", "val_numeric_target") %in%
            names(selection))) {
      survey <- relabel_survey(survey, selection)
    }
    
    if (!"class_target" %in% names(selection)) {
      return(survey)
    }
    
    survey %>%
      dplyr::mutate(
        dplyr::across(
          selection$var_name_target[selection$class_target == "factor"],
          as_factor
        ),
        dplyr::across(
          selection$var_name_target[selection$class_target == "character"],
          as_character
        ),
        dplyr::across(
          selection$var_name_target[selection$class_target == "numeric"],
          as_numeric
        )
      )
  }
  
  # ------------------------------------------------------------------
  # 5. Apply safely to all surveys
  # ------------------------------------------------------------------
  results <- purrr::map(harmonized_surveys, purrr::safely(harmonize_one_survey))
  
  errors <- purrr::map_chr(results, ~ if (is.null(.x$error)) "" else .x$error$message)
  
  if (any(errors != "")) {
    error_table <- tibble::tibble(
      id = vapply(harmonized_surveys, function(x) attr(x, "id"), character(1)),
      error = errors
    )
    
    message("The following surveys could not be harmonized:")
    print(error_table)
    stop("crosswalk_surveys() failed.", call. = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 6. Return harmonized surveys
  # ------------------------------------------------------------------
  purrr::map(results, "result")
}

#' @title Validate a crosswalk table
#' @rdname crosswalk_table_create
#' @param ctable A table to validate if it is a crosswalk table.
#' @importFrom dplyr tally group_by across filter distinct
#' @family metadata functions
#' @export

is.crosswalk_table <- function(ctable) {
  assert_that(inherits(ctable, "data.frame"),
              msg = "The cross table should be a data frame object (data.frame, tibble, or similar.)"
  )
  
  assert_that(
    all(c("id", "var_name_orig", "var_name_target") %in% names(ctable)),
    msg = "The crosstable must have at least an 'id' and a 'var_name_orig', 'var_name_target' columns."
  )
  
  duplicates <- ctable %>%
    dplyr::select(all_of(c("id", "var_name_orig", "var_name_target"))) %>%
    dplyr::distinct(id, var_name_orig, var_name_target, .keep_all = TRUE) %>%
    dplyr::group_by(across(c("var_name_target", "id"))) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(var_name_target) %>%
    unlist()
  
  error_msg <- paste(unique(duplicates), collapse = ", ")
  if (length(duplicates) == 0) error_msg <- ""
  
  assert_that(
    length(duplicates) == 0,
    msg = glue("The crosstable '{deparse(substitute(ctable))}' has the following non-unique target variables: {error_msg}.")
  )
}

#' @title Create a crosswalk table
#'
#' @description Create a crosswalk table with the source variable names
#' and variable labels.
#'
#' @details The table contains a \code{var_name_target} and 
#' \code{val_label_target} column, but
#' these values need to be set by further manual or 
#' reproducible harmonization steps.
#'
#' @param metadata A metadata table created by 
#' [metadata_create()].
#' @return A tibble with raw crosswalk table. It contains all 
#' harmonization tasks, but the
#' target values need to be set by further manipulations.
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @family harmonization functions
#' @export

crosswalk_table_create <- function(metadata) {
  assert_that(inherits(metadata, "data.frame"),
              msg = "Parameter 'metadata' must be a data frame object."
  )
  
  assert_that(nrow(metadata) >= 1,
              msg = "The 'metadata' data frame must have at least one row."
  )
  
  compulsory_columns <- c("filename", "id", "var_name_orig", "labels")
  
  missing_columns <- setdiff(compulsory_columns, names(metadata))
  
  assert_that(
    length(missing_columns) == 0,
    msg = glue("The 'metadata' data frame has missing columns: {paste(missing_columns, collapse = ", "}")
  )
  
  fn_labels <- function(x) {
    if (is.na(x$labels)) {
      # The variable is not labelled
      label_length <- 1
      tibble(
        id = rep(unique(x$id), label_length),
        filename = rep(unique(x$filename), label_length),
        var_name_orig = rep(x$var_name_orig, label_length),
        var_name_target = rep(x$var_name_orig, label_length),
        val_numeric_orig = NA_real_,
        val_numeric_target = NA_real_,
        val_label_orig = NA_character_,
        val_label_target = NA_character_,
        class_orig = rep(unique(x$class_orig), label_length),
        class_target = rep(unique(x$class_orig), label_length)
      )
    } else {
      val_labels <- names(unlist(x$labels))
      label_length <- length(unlist(x$labels))
      
      tmp <- tibble(
        id = rep(unique(x$id), label_length),
        filename = rep(unique(x$filename), label_length),
        var_name_orig = rep(x$var_name_orig, label_length),
        var_name_target = rep(x$var_name_orig, label_length),
        val_numeric_orig = as.numeric(unlist(x$labels)),
        val_numeric_target = as.numeric(unlist(x$labels)),
        val_label_orig = as.character(vapply(x$labels, names, character(label_length))),
        val_label_target = as.character(vapply(x$labels, names, character(label_length)))
      )
      
      if ("na_labels" %in% names(x)) {
        na_labels <- names(unlist(x$na_labels))
        
        tmp$na_label_orig <- ifelse(tmp$val_label_orig %in% na_labels,
                                    tmp$val_label_orig, NA_character_
        )
        tmp$na_label_target <- tmp$na_label_orig
        
        tmp$na_numeric_orig <- ifelse(tmp$val_label_orig %in% na_labels,
                                      tmp$val_numeric_orig, NA_real_
        )
        tmp$na_numeric_target <- tmp$na_numeric_orig
      } else {
        tmp$na_label_orig <- NA_character_
        tmp$na_label_target <- NA_character_
        tmp$na_numeric_orig <- NA_character_
        tmp$na_numeric_target <- NA_real_
      }
      
      if ("var_label_orig" %in% names(x)) {
        tmp$var_label_orig <- x$var_label_orig
        tmp$var_label_target <- tmp$var_label_orig
      } else {
        tmp$var_label_orig <- NA_character_
        tmp$var_label_target <- NA_character_
      }
      if ("class_orig" %in% names(x)) {
        tmp <- tmp %>%
          left_join(x %>% select(all_of(c("var_name_orig", "class_orig"))),
                    by = "var_name_orig"
          )
        
        tmp$class_target <- case_when(
          tmp$class_orig %in% c("numeric", "character") ~ tmp$class_orig,
          TRUE ~ "factor"
        )
      } else {
        tmp$class_orig <- NA_character_
        tmp$class_target <- NA_character_
      }
      tmp
    }
  }
  
  if (nrow(metadata) == 1) {
    fn_labels(x = metadata[1, ])
  } else {
    ctable_list <- lapply(1:nrow(metadata), function(x) fn_labels(metadata[x, ]))
    ctable <- suppressMessages(purrr::reduce(ctable_list, full_join))
    ctable
  }
}

#' @rdname crosswalk_surveys
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @export
crosswalk <- function(survey_list, 
                      crosswalk_table, 
                      na_values = NULL) {
  
  crosswalked <- crosswalk_surveys(
    survey_list = survey_list,
    crosswalk_table = crosswalk_table,
    na_values = na_values
  )
  
  if (!is.null(crosswalked) & length(crosswalked) > 1) {
    suppressMessages(purrr::reduce(crosswalked, full_join))
  } else {
    crosswalked
  }
}
