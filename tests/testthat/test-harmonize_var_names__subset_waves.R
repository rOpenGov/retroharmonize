
examples_dir <- system.file("examples", package = "retroharmonize")
survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]

example_surveys <- read_surveys(
  file.path( examples_dir, survey_list), 
  save_to_rds = FALSE)

metadata <- lapply ( X = example_surveys, FUN = metadata_create )
metadata <- do.call(rbind, metadata)

metadata$var_name <- label_normalize(metadata$var_name)

metadata$var_name [metadata$label_orig == "age education"] <- "age_education"

hnw <- harmonize_var_names(waves = example_surveys, 
                           metadata = metadata )

test_that("renaming works", {
  expect_equal(unlist(lapply ( hnw, function(x) "age_education" %in% names(x))), rep(TRUE, 3))
})

snw <- subset_waves (hnw, subset_names = c("uniqid", "w1", "age_education"))


test_that("subsetting works", {
  expect_equal(unique(sapply ( unlist(lapply ( snw, names )), c)), 
               c("uniqid", "w1", "age_education"))
})


