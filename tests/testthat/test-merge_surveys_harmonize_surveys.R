test_that("merge_surveys returns a list of surveys", {
  
  examples_dir <- system.file("examples", package = "retroharmonize")
  rds_files <- dir(examples_dir, pattern = "\\.rds$")
  surveys <- read_surveys(file.path(examples_dir, rds_files))
  
  metadata <- metadata_create(surveys)
  
  to_harmonize <- metadata %>%
    filter(
      var_name_orig %in% c("rowid", "w1") |
        grepl("^trust", var_label_orig)
    ) %>%
    mutate(
      var_label = var_label_normalize(var_label_orig),
      var_name_target = val_label_normalize(var_label_orig),
      var_name_target = ifelse(
        var_name_orig %in% c("rowid", "w1", "wex"),
        var_name_orig,
        var_name_target
      )
    )
  
  merged <- merge_surveys(
    survey_list = surveys,
    var_harmonization = to_harmonize
  )
  
  expect_type(merged, "list")
  expect_length(merged, length(surveys))
})


test_that("harmonize_survey_values returns labelled SPSS variables", {
  
  harmonize_eb_trust <- function(x) {
    label_list <- list(
      from = c(
        "^tend\\snot", "^cannot", "^tend\\sto", "^can\\srely",
        "^dk", "^inap", "na"
      ),
      to = c(
        "not_trust", "not_trust", "trust", "trust",
        "do_not_know", "inap", "inap"
      ),
      numeric_values = c(0, 0, 1, 1, 99997, 99999, 99999)
    )
    
    harmonize_values(
      x,
      harmonize_labels = label_list,
      na_values = c(
        "do_not_know" = 99997,
        "declined" = 99998,
        "inap" = 99999
      )
    )
  }
  
  examples_dir <- system.file("examples", package = "retroharmonize")
  rds_files <- dir(examples_dir, pattern = "\\.rds$")
  surveys <- read_surveys(file.path(examples_dir, rds_files))
  
  metadata <- metadata_create(surveys)
  
  to_harmonize <- metadata %>%
    dplyr::filter(
      var_name_orig %in% c("rowid", "w1") |
        grepl("^trust", var_label_orig)
    ) %>%
    dplyr::mutate(
      var_label = var_label_normalize(var_label_orig),
      var_name_target = val_label_normalize(var_label_orig),
      var_name_target = ifelse(
        var_name_orig %in% c("rowid", "w1", "wex"),
        var_name_orig,
        var_name_target
      )
    )
  
  merged <- merge_surveys(
    survey_list = surveys,
    var_harmonization = to_harmonize
  )
  
  harmonized <- suppressWarnings(
    harmonize_survey_values(
      survey_list = merged,
      .f = harmonize_eb_trust,
      status_message = FALSE
    )
  )
  
  expect_true(
    is.labelled_spss_survey(
      harmonized$trust_in_institutions_army
    )
  )
})


test_that("harmonized trust variables have expected numeric range", {
  
  examples_dir <- system.file("examples", package = "retroharmonize")
  rds_files <- dir(examples_dir, pattern = "\\.rds$")
  surveys <- read_surveys(file.path(examples_dir, rds_files))
  
  metadata <- metadata_create(surveys)
  
  to_harmonize <- metadata %>%
    dplyr::filter(
      var_name_orig %in% c("rowid", "w1") |
        grepl("^trust", var_label_orig)
    ) %>%
    dplyr::mutate(
      var_label = var_label_normalize(var_label_orig),
      var_name_target = val_label_normalize(var_label_orig),
      var_name_target = ifelse(
        var_name_orig %in% c("rowid", "w1", "wex"),
        var_name_orig,
        var_name_target
      )
    )
  
  merged <- merge_surveys(
    survey_list = surveys,
    var_harmonization = to_harmonize
  )
  
  harmonize_eb_trust <- function(x) { 
    label_list <- list( from = c( "^tend\\snot", "^cannot", "^tend\\sto",
                                  "^can\\srely", "^dk", "^inap", "na" ), 
                        to = c( "not_trust", "not_trust", "trust", "trust",
                                "do_not_know", "inap", "inap" ), 
                        numeric_values = c(0, 0, 1, 1,
                                           99997, 99999, 99999) ) 
    
    harmonize_values(x, harmonize_labels = label_list, 
                     na_values = c( "do_not_know" = 99997, 
                                    "declined" = 99998, 
                                    "inap" = 99999 ))
    }
  
  harmonized <- suppressWarnings(
    harmonize_survey_values(
      survey_list = merged,
      .f = function(x) harmonize_eb_trust(x),
      status_message = FALSE
    )
  )
 
  expect_equal(
    range(
      as_numeric(
        harmonized$trust_in_institutions_european_union
      )[as_numeric(
        harmonized$trust_in_institutions_european_union
      ) < 99997],
      na.rm = TRUE
    ),
    c(0, 1)
  )
})

