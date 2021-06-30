examples_dir <- system.file( "examples", package = "retroharmonize")

my_rds_files <- file.path(examples_dir, dir( examples_dir)[grepl(".rds", 
                                         dir(examples_dir))])

example_surveys <- read_surveys(import_file_names = my_rds_files)

test_that("All files are read", {
  expect_equal(length(example_surveys), 3)
})

wrong_files <- c(file.path(examples_dir), "no_file.rds", example_surveys)

test_that("exception handling works", {
  expect_error(read_surveys(wrong_files))
})


  