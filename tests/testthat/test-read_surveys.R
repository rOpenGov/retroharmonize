test_that("read_surveys() reads the correct file", {
  examples_dir <- system.file("examples", package = "retroharmonize")
  my_rds_files <- file.path(examples_dir, dir(examples_dir)[grepl(
    ".rds",
    dir(examples_dir)
  )])
  example_surveys <- read_surveys(survey_paths = my_rds_files)
  expect_equal(
    attr(read_survey(file_path = my_rds_files[1]), "filename"),
    fs::path_file(my_rds_files[1])
  )
})

test_that("read_surveys() error if file does not exists", {
  expect_error(read_surveys(tempfile(), .f = "read_csv", export_path = NULL))
})


test_that("read_surveys() reads all files", {
  examples_dir <- system.file("examples", package = "retroharmonize")
  my_rds_files <- file.path(examples_dir, dir(examples_dir)[grepl(
    ".rds",
    dir(examples_dir)
  )])
  example_surveys <- read_surveys(survey_paths = my_rds_files)
  expect_equal(length(example_surveys), 3)
})
