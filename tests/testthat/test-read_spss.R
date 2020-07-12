path <- system.file("examples", "iris.sav", package = "haven")
read_iris <- read_spss(
  file = path, id = "iris", 
  filename = "iris.sav")

test_that("attributes work", {
  expect_equal(attr(read_iris, "id"), "iris")
  expect_equal(attr(read_iris, "filename"), "iris.sav")
  expect_equal(attr(read_iris, "doi"), NULL)
})


