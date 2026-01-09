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
  # Check if the crosswalk_table can be used
  is.crosswalk_table(crosswalk_table)

  # Remove surveys that are not used in the crosswalk table ---------------------------------
  available_surveys <- unlist(lapply(survey_list, 
                                     function(x) attr(x, "id"))
                              )
  surveys_to_harmonize <- unique(crosswalk_table$id)
  survey_list[!available_surveys %in% surveys_to_harmonize] <- NULL

  # See if the na_values are given in the crosswalk_table -----------------------------------
  if (!all(c("na_numeric_target", "na_label_target") %in% 
           names(crosswalk_table))) {
    if (!is.null(na_values)) {
      assert_that(is.character(na_values) | is.numeric(na_values),
        msg = "Parameter 'na_values' must be a named character or numeric string, it is not this type"
      )

      assert_that(length(names(na_values)) > 0,
        msg = "Parameter 'na_values' must be a named character or numeric string, but it has no names."
      )
    } else {
      set_na_values <- NULL
    }
  }

  ## Harmonize the variable names and remove not harmonized vars -------------------
  harmonized_survey_vars <- harmonize_survey_variables(
    survey_list = survey_list,
    survey_paths = survey_paths,
    import_path = import_path,
    crosswalk_table = crosswalk_table
  )

  ## Relabel surveys -------------------------------------------------
  relabel_survey <- function(y, selection) {
    assert_that(inherits(selection, "data.frame"),
      msg = "selectin must be a data.frame"
    )

    assert_that(nrow(selection) > 0,
      msg = "selection must have rows"
    )

    select_to_harmonize <- selection %>%
      filter(!is.na(val_label_orig))

    vars_to_harmonize <- unique(select_to_harmonize$var_name_target)

    if (length(vars_to_harmonize) == 0) {
      return(y)
    }

    # this_var <- vars_to_harmonize[2]
    return_value <- y

    for (this_var in vars_to_harmonize) {
      correspondence_table <- select_to_harmonize %>%
        filter(var_name_target == this_var)

      assert_that(is.numeric(correspondence_table$val_numeric_target),
        msg = "Error in relabel_survey: 'val_numeric_target' must be a numeric vector"
      )

      if (all(c("na_numeric_target", "na_label_target") %in% names(correspondence_table))) {
        set_na_values <- correspondence_table$na_numeric_target[!is.na(correspondence_table$na_numeric_target)]
        names(set_na_values) <- correspondence_table$na_label_target[!is.na(correspondence_table$na_label_target)]
      }

      if (length(set_na_values) == 0) set_na_values <- NULL

      harmonize_these_labels <- function(z) {
        harmonize_labels <- list(
          from = paste0("^", correspondence_table$val_label_orig, "$"),
          to = correspondence_table$val_label_target,
          numeric_values = correspondence_table$val_numeric_target
        )


        harmonize_values(
          x = z,
          harmonize_labels = harmonize_labels,
          na_values = set_na_values
        )
      }
      return_value <- return_value %>%
        mutate(across(any_of(this_var), harmonize_these_labels))
    }

    return_value
  }

  subset_survey <- function(this_survey) {
    survey_id <- attr(this_survey, "id")
    assert_that(length(survey_id) > 0,
      msg = "Error in subset_survey(): survey_id has 0 length."
    )

    tmp <- this_survey %>%
      mutate(id = survey_id) %>%
      relocate(id, .before = everything())

    selection <- crosswalk_table %>%
      filter(id == survey_id) %>%
      distinct_all()


    if ( # label harmonization is possible
      all(c("val_label_orig", "val_label_target", "val_numeric_target") %in% names(selection))
    ) {
      tmp <- relabel_survey(y = tmp, selection = selection)
    }


    ## At last harmonnize class if possible-----------------------------

    if (!"class_target" %in% names(selection)) {
      return(tmp)
    }

    factor_vars <- selection$var_name_target[which(selection$class_target == "factor")]
    character_vars <- selection$var_name_target[which(selection$class_target == "character")]
    numeric_vars <- selection$var_name_target[which(selection$class_target == "numeric")]

    return_df <- tmp %>%
      mutate(
        across(any_of(factor_vars), as_factor),
        across(any_of(character_vars), as_character),
        across(any_of(numeric_vars), as_numeric)
      )

    return_df
  }
  x <- this_survey <- harmonized_survey_vars[[2]]
  subsetted <- lapply(harmonized_survey_vars, function(x) purrr::safely(subset_survey)(x))

  errors <- lapply(subsetted, function(x) x$error)

  error_table <- tibble(
    id = as.character(lapply(survey_list, function(x) attr(x, "id"))),
    error = as.character(lapply(errors, function(x) ifelse(is.null(x), "", x$message)))
  )

  if (!all(error_table$error == "")) {
    message("The following surveys were could not be harmonized:")
    print(error_table)
    stop("This is an error from function crosswalk_surveys()")
  }

  results <- lapply(subsetted, function(x) x$result)
  results[vapply(results, is.null, logical(1))] <- NULL
  results
}

#' @rdname crosswalk_surveys
#' @export
crosswalk <- function(survey_list, crosswalk_table, na_values = NULL) {
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

#' @title Create a crosswalk table
#'
#' @description Create a crosswalk table with the source variable names and variable labels.
#'
#' @details The table contains a \code{var_name_target} and \code{val_label_target} column, but
#' these values need to be set by further manual or reproducible harmonization steps.
#'
#' @param metadata A metadata table created by 
#' \code{\link{metadata_create}}.
#' @return A tibble with raw crosswalk table. It contains all harmonization tasks, but the
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
