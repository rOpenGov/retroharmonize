path <- here("inst", "examples")

read_example <- read_spss(
  file = file.path(path, "iris1.sav"),
  id = "my_iris")

test_that("attributes work", {
  expect_equal(attr(read_example, "id"), "my_iris")
  expect_equal(attr(read_example, "filename"), "iris1.sav")
  expect_equal(attr(read_example, "doi"), NULL)
})


