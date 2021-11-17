examples_dir <- system.file("examples", package = "retroharmonize")
survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]

example_surveys <- read_surveys(
  file.path( examples_dir, survey_list), 
  save_to_rds = FALSE)

metadata <- metadata_surveys_create(example_surveys)
metadata$var_name_suggested <- label_normalize(metadata$var_name)
metadata$var_name_suggested[metadata$label_orig == "age_education"] <- "age_education"

hnw <- harmonize_var_names(survey_list = example_surveys, 
                           metadata = metadata )

test_that("renaming works", {
  expect_equal(unlist(lapply ( hnw, function(x) "age_education" %in% names(x))), rep(TRUE, 3))
})

snw <- subset_surveys (hnw, subset_names = c("uniqid", "w1", "age_education"))

test_that("subsetting works", {
  expect_true(all ( unique(unlist(lapply(snw,names))) %in% c("rowid", "uniqid", "w1", "age_education"))
)
})


