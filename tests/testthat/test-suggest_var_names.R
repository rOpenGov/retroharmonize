test_that("suggest_permanent_names-returns-proper-class", {
  expect_equal(class(suggest_permanent_names("eurobarometer")), "character" )
  expect_equal(suggest_permanent_names("pseudobarometer"), NA_character_)
})

examples_dir <- system.file("examples", package = "retroharmonize")
survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]

example_surveys <- read_surveys(
  file.path( examples_dir, survey_list), 
  export_path = NULL)

metadata <- metadata_create (example_surveys)

suggested <- suggest_var_names(metadata, survey_program = "eurobarometer")

test_that("multiplication works", {
  expect_true( all(c("rowid", "isocntry") %in% suggested$var_name_suggested ) )
})
