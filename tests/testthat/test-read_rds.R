examples_dir <- system.file( "examples", package = "retroharmonize")

test_read <- read_rds ( file.path(examples_dir, "ZA7576.rds"),
                        id = "ZA7576", 
                        doi = "test_doi")

test_that("attributes work", {
  expect_equal(attr(test_read, "id"), "ZA7576")
  expect_equal(attr(test_read, "filename"), "ZA7576.rds")
  expect_equal(attr(test_read, "doi"), "test_doi")
})

read_write_test <- function() {
  temp_save_location <- tempdir()
  write.csv(test_read, file.path(temp_save_location, "test.csv"), row.names = F)
  is.survey(read_csv (file = file.path(temp_save_location, "test.csv")))
}

test_that("read_csv", {
  skip_on_cran()
  expect_true(read_write_test())
})


