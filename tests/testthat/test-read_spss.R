test_that("read_spss_survey() with attributes work", {
  examples_dir <- system.file( "examples", package = "retroharmonize")
  read_example <- read_spss(
    file = file.path(examples_dir, "iris1.sav"),
    id = "my_iris")
  expect_equal(attr(read_example, "id"), "my_iris")
  expect_equal(attr(read_example, "filename"), "iris1.sav")
  expect_equal(attr(read_example, "doi"), NULL)
})

test_that("exception handling works", {
  expect_error ( read_spss (file.path(examples_dir, "not_iris.sav")))
})

