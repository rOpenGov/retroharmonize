test_that("validate_survey_list", {
  expect_error(metadata_surveys_create(survey_list = data.frame()))
})
