
survey_list <- dir (
  here::here( "inst", "examples"))[grepl(".rds", 
                                         dir (here( "inst", "examples")))]

example_surveys <- read_surveys(
  here::here( "inst", "examples", survey_list))


test_that("pulling works", {
  expect_equal(nrow(pull_survey(example_surveys, id = "ZA5913")), 35)
  expect_equal(nrow(pull_survey(example_surveys, filename = "ZA5913.rds")), 35)
})
