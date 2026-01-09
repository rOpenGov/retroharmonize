examples_dir <- system.file("examples", package = "retroharmonize")
survey_files <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
example_surveys <- read_surveys(
  file.path(examples_dir, survey_files),
  export_path = NULL
)

subset_survey_list_1 <- subset_surveys(
  survey_list = example_surveys,
  subset_vars = c("rowid", "isocntry", "qa10_1", "qa14_1"),
  subset_name = "subset_example"
)

test_that("saving and subsetting (not on CRAN)", {
  expect_equal(
    vapply(subset_survey_list_1, function(x) attr(x, "id"), character(1)),
    c("ZA5913", "ZA6863", "ZA7576")
  )
  expect_equal(
    vapply(subset_survey_list_1, function(x) attr(x, "filename"), character(1)),
    c("ZA5913_subset_example.rds", "ZA6863_subset_example.rds", "ZA7576_subset_example.rds")
  )
  expect_equal(vapply(subset_survey_list_1, ncol, numeric(1)), c(3, 3, 3))
})


example_metadata <- metadata_create(
  survey_paths = file.path(examples_dir, survey_files),
  .f = "read_rds"
)

example_ctable <- example_metadata %>%
  filter(.data$var_name_orig %in% c("rowid", "isocntry", "d60", "wex")) %>%
  crosswalk_table_create()

crosswalk_table <- example_ctable
survey_paths <- file.path(examples_dir, survey_files)

testing_subsetting <- function() {
  test_survey <- read_rds(
    file = system.file("examples", "ZA7576.rds",
      package = "retroharmonize"
    )
  )

  test_metadata <- metadata_create(test_survey)
  test_metadata <- test_metadata[c(1, 7, 18), ]
  ctable <- crosswalk_table_create(test_metadata)
  ctable$var_name_target <- ifelse(ctable$var_name_orig == "qa14_3",
    "trust_ecb",
    ctable$var_name_orig
  )

  temporary_directory <- tempdir()

  temporary_saving_location <- file.path(temporary_directory, "ZA7576.rds")

  saveRDS(test_survey, temporary_saving_location, version = 2)

  file_result <- subset_surveys(
    crosswalk_table = ctable,
    survey_list = test_survey,
    subset_name = "tested",
    survey_paths = NULL,
    import_path = NULL,
    export_path = tempdir()
  )


  file_result == "ZA7576_tested.rds" &
    file.exists(file.path(tempdir(), "ZA7576_tested.rds"))
}

res <- evaluate_promise(testing_subsetting())

test_that("saving and subsetting", {
  skip_on_cran()
  expect_true("Saving ZA7576_tested.rds\n" %in% evaluate_promise(testing_subsetting())$messages)
})

test_that("saving and subsetting (not on CRAN)", {
  skip_on_cran()
  expect_true(ncol(readRDS(file.path(tempdir(), "ZA7576_tested.rds"))) == 3)
  expect_error(save_surveys(
    crosswalk_table = ctable,
    subset_name = "tested",
    import_path = NULL,
    export_path = NULL
  ))
})
