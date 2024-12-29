# See also test-read_surveys
examples_dir <- system.file( "examples", package = "retroharmonize")
test_csv_file <- tempfile()

test_that("read_csv works", {
  test_read <- read_rds ( file.path(examples_dir, "ZA7576.rds"),
                          id = "ZA7576", 
                          doi = "test_doi")
  write.csv(x = test_read, file = test_csv_file, row.names = F)
  re_read <- read_csv(test_csv_file, id = "ZA7576", doi = "test_doi")
  expect_equal(attr(re_read, "doi"), "test_doi")
  expect_equal(attr(re_read, "id"), "ZA7576")
  expect_true(is.survey(re_read))
})

test_that("read_survey(...) passes on ...", {
  re_read_2 <- read_survey(file_path = test_csv_file, 
                           .f = "read_csv",
                           id = "ZA7576", 
                           doi = "test_doi")
  expect_equal(attr(re_read_2, "doi"), "test_doi")
  expect_equal(attr(re_read_2, "id"), "ZA7576")
  expect_true(is.survey(re_read_2))
})


test_that("read_surveys(...) passes on ...", {
  re_read_3 <- read_surveys(survey_paths = test_csv_file, 
                            .f = "read_csv",
                            ids = "ZA7576", 
                            dois = "test_doi")[[1]]
  expect_equal(attr(re_read_3, "doi"), "test_doi")
  expect_equal(attr(re_read_3, "id"), "ZA7576")
  expect_true(is.survey(re_read_3))
})
