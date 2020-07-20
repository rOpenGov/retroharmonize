example_survey <- survey( 
  df =data.frame ( 
    rowid = 1:6,
    observations = runif(6)), 
  id = 'example', 
  filename = "no_file"
)

test_that("correct casint", {
  expect_true(inherits(example_survey, "survey"))
  expect_equal(attr(example_survey, "filename"), "no_file")
})
