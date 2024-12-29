

test_that("attributes work", {
  examples_dir <- system.file( "examples", package = "retroharmonize")
  test_read <- read_rds ( file.path(examples_dir, "ZA7576.rds"),
                          id = "ZA7576", 
                          doi = "test_doi")
  expect_equal(attr(test_read, "id"), "ZA7576")
  expect_equal(attr(test_read, "filename"), "ZA7576.rds")
  expect_equal(attr(test_read, "doi"), "test_doi")
})

read_write_test <- function() {
  examples_dir <- system.file( "examples", package = "retroharmonize")
  test_read <- read_rds ( file.path(examples_dir, "ZA7576.rds"),
                          id = "ZA7576", 
                          doi = "test_doi")
  temp_save_location <- tempdir()
  write.csv(test_read, file.path(temp_save_location, "test.csv"), row.names = F)
  reread <- read_csv(file = file.path(temp_save_location, "test.csv"))
  is.survey()
}


