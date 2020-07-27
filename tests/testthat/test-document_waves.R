
survey_list <- dir (
  here::here( "inst", "examples"))[grepl(".rds", 
                                         dir (here( "inst", "examples")))]

example_surveys <- read_surveys(
  here::here( "inst", "examples", survey_list))

documented <- document_waves ( example_surveys )

test_that("multiplication works", {
  expect_equal(documented$id, c("ZA5913", "ZA6863", "ZA7576"))
})
