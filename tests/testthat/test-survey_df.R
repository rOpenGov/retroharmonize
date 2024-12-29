example_survey <- survey_df( 
  data.frame( 
    rowid = 1:6,
    observations = runif(6)), 
  identifier = 'example', 
  filename = "no_file"
)

test_that("survey_df() works", {
  expect_true(is.numeric(example_survey$observations))
  expect_equal(attr(example_survey, "id"), "example")
  expect_equal(attr(example_survey, "filename"), "no_file")
})
