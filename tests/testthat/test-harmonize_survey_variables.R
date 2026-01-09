examples_dir <- system.file("examples", package = "retroharmonize")
survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
example_surveys <- read_surveys(
  file.path(examples_dir, survey_list),
  export_path = NULL
)

documented_surveys <- metadata_create(example_surveys)
documented_surveys <- documented_surveys[
  documented_surveys$var_name_orig %in% c(
    "rowid", "isocntry", "w1", "qd3_4",
    "qd3_8", "qd7.4", "qd7.8", "qd6.4", "qd6.8"
  ),
]
crosswalk_table <- crosswalk_table_create(metadata = documented_surveys)

freedom_table <- crosswalk_table[
  which(crosswalk_table$var_name_target %in% c("rowid", "freedom")),
]

harmonized <- harmonize_survey_variables(
  crosswalk_table = freedom_table,
  subset_name = "freedom",
  survey_list = example_surveys
)


test_that("harmonize_survey_variables() subsets and renames as expected", {
  expect_true(is.list(harmonized))
})
