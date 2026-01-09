test_that("merge_surveys example runs without error", {
  
  examples_dir <- system.file("examples", package = "retroharmonize")
  survey_files <- dir(examples_dir, pattern = "\\.rds$", full.names = TRUE)
  
  example_surveys <- read_surveys(
    survey_files,
    save_to_rds = FALSE
  )
  
  metadata <- metadata_create(survey_list = example_surveys)
  
  to_harmonize <- metadata %>%
    dplyr::filter(
      var_name_orig %in% c("rowid", "w1") |
        grepl("^trust", var_label_orig)
    ) %>%
    dplyr::mutate(
      var_label = var_label_normalize(var_label_orig),
      var_name_target = val_label_normalize(var_label),
      var_name_target = ifelse(
        .data$var_name_orig %in% c("rowid", "w1", "wex"),
        .data$var_name_orig,
        .data$var_name_target
      )
    )
  
  expect_no_error(
    merge_surveys(
      survey_list = example_surveys,
      var_harmonization = to_harmonize
    )
  )
})
