test_that("create_codebook works with metadata input", {
  
  survey <- read_rds(
    system.file("examples", "ZA7576.rds", package = "retroharmonize")
  )
  
  metadata <- metadata_create(survey)
  metadata$user_var <- paste0(seq_len(nrow(metadata)), "_user")
  
  codebook <- create_codebook(metadata = metadata)
  
  expect_s3_class(codebook, "data.frame")
  expect_true("user_var" %in% names(codebook))
})


test_that("create_codebook works with survey input", {
  
  survey <- read_rds(
    system.file("examples", "ZA7576.rds", package = "retroharmonize")
  )
  
  codebook <- create_codebook(survey = survey)
  
  expect_s3_class(codebook, "data.frame")
  expect_true(all(
    c("entry", "val_code_orig", "val_label_orig", "label_range") %in%
      names(codebook)
  ))
})


test_that("codebook_waves_create emits deprecation warning", {
  
  examples_dir <- system.file("examples", package = "retroharmonize")
  survey_files <- dir(examples_dir, pattern = "\\.rds$")
  surveys <- read_surveys(
    file.path(examples_dir, survey_files),
    export_path = NULL
  )
  
  expect_warning(
    codebook_waves_create(waves = surveys),
    class = "deprecatedWarning"
  )
})


test_that("codebook_surveys_create combines surveys correctly", {
  
  examples_dir <- system.file("examples", package = "retroharmonize")
  survey_files <- dir(examples_dir, pattern = "\\.rds$")
  surveys <- read_surveys(
    file.path(examples_dir, survey_files),
    export_path = NULL
  )
  
  codebook <- codebook_surveys_create(survey_list = surveys)
  
  expect_s3_class(codebook, "data.frame")
  expect_true(all(survey_files %in% unique(codebook$filename)))
  expect_true(all(c("valid", "missing") %in% unique(codebook$label_range)))
})


test_that("create_codebook fails with invalid input", {
  
  expect_error(
    create_codebook(survey = data.frame()),
    regexp = "Parameter survey must be of class survey"
  )
  
  expect_error(
    create_codebook(metadata = NULL, survey = NULL), 
    regexp = "Either the parameter 'metadata' or the parameter 'survey' must be given"
  )
})

