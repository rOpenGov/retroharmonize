examples_dir <- system.file("examples", package = "retroharmonize")

my_rds_files <- dir(examples_dir)[grepl(
  ".rds",
  dir(examples_dir)
)]

example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))

test_that("pulling works", {
  expect_equal(nrow(pull_survey(example_surveys, id = "ZA5913")), 35)
  expect_equal(nrow(pull_survey(example_surveys, filename = "ZA5913.rds")), 35)
})
