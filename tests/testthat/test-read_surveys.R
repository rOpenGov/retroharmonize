examples_dir <- system.file( "examples", package = "retroharmonize")

my_rds_files <- file.path(examples_dir, dir( examples_dir)[grepl(".rds", 
                                         dir(examples_dir))])

example_surveys <- read_surveys(survey_paths = my_rds_files)

test_that("read_surveys() reads the correct file", {
  expect_equal(attr(read_survey( file_path = my_rds_files[1]), "filename"), fs::path_file(my_rds_files[1]))
})

test_that("read_surveys() error if file does not exists", {
  expect_error(read_surveys(tempfile(), .f = "read_csv", export_path = NULL))
})


test_that("read_surveys() reads all files", {
  expect_equal(length(example_surveys), 3)
})

# -------------------- read_csv ----------------------------
examples_dir <- system.file( "examples", package = "retroharmonize")

test_read <- read_rds ( file.path(examples_dir, "ZA7576.rds"),
                        id = "ZA7576", 
                        doi = "test_doi")
test_csv_file <- tempfile()
write.csv(x = test_read, file = test_csv_file, row.names = F)
re_read <- read_csv(test_csv_file, id = "ZA7576", doi = "test_doi")

test_that("read_csv works", {
  expect_equal(attr(re_read, "doi"), "test_doi")
  expect_equal(attr(re_read, "id"), "ZA7576")
  expect_true(is.survey(re_read))
})

re_read_2 <- read_survey(file_path = test_csv_file, 
                         .f = "read_csv",
                         id = "ZA7576", 
                         doi = "test_doi")

test_that("read_survey(...) passes on ...", {
  expect_equal(attr(re_read_2, "doi"), "test_doi")
  expect_equal(attr(re_read_2, "id"), "ZA7576")
  expect_true(is.survey(re_read_2))
})


re_read_3 <- read_surveys(survey_paths = test_csv_file, 
                          .f = "read_csv",
                          ids = "ZA7576", 
                          dois = "test_doi")[[1]]

test_that("read_surveys(...) passes on ...", {
  expect_equal(attr(re_read_3, "doi"), "test_doi")
  expect_equal(attr(re_read_3, "id"), "ZA7576")
  expect_true(is.survey(re_read_3))
})
