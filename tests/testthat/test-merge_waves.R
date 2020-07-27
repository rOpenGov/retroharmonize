require(dplyr)
survey_list <- dir (
  here::here( "inst", "examples"))[grepl(".rds", 
                                         dir (here( "inst", "examples")))]

example_surveys <- read_surveys(
  here::here( "inst", "examples", survey_list))

metadata <- lapply ( X = example_surveys, FUN = metadata_create )
metadata <- do.call(rbind, metadata)

to_harmonize <- metadata %>%
  filter ( var_name_orig %in% 
             c("rowid", "w1") |
             grepl("trust ", label_orig ) ) %>%
  mutate ( var_label = var_label_normalize(label_orig)) %>%
  mutate ( var_name = val_label_normalize(var_label))

test_that("correct structure is returned", {
  expect_equal(length(
    merge_waves ( example_surveys, to_harmonize )), 3)
  expect_true(is.list(
    merge_waves ( example_surveys, to_harmonize )))
})
