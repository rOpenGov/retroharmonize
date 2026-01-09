test_that("correct casting", {
  example_survey <- survey(
    object = data.frame(
      rowid = 1:6,
      observations = runif(6)
    ),
    id = "example",
    filename = "no_file"
  )
  expect_s3_class(example_survey, "survey")
  expect_true(is.survey(example_survey))
  expect_equal(attr(example_survey, "filename"), "no_file")
})
