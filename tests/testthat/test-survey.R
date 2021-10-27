example_survey <- survey( 
  df =data.frame ( 
    rowid = 1:6,
    observations = runif(6)), 
  id = 'example', 
  filename = "no_file"
)

test_that("correct casting", {
  expect_true(inherits(example_survey, "survey"))
  expect_true(is.survey(example_survey))
  expect_equal(attr(example_survey, "filename"), "no_file")
})


test_that("summary.survey generic produces the same results", {
  expect_true(inherits(example_survey, "survey"))
  expect_true(is.survey(example_survey))
  
  positional <- capture_output(summary(example_survey))
  named <- capture_output(summary(df = example_survey))
  
  expect_identical(named, positional)
  
  # Use named object
  namedobj <- capture_output(summary(object = example_survey))
  expect_identical(named, namedobj)
  
})
