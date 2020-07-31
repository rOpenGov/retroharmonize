require(dplyr)
library(vctrs)
library(rlang)
examples_dir <- system.file( "examples", package = "retroharmonize")
my_rds_files <- dir( examples_dir)[grepl(".rds", 
                                         dir(examples_dir))]
example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))

metadata <- lapply ( X = example_surveys, FUN = metadata_create )
metadata <- do.call(rbind, metadata)

to_harmonize <- metadata %>%
  filter ( var_name_orig %in% 
             c("rowid", "w1") |
             grepl("trust ", label_orig ) ) %>%
  mutate ( var_label = var_label_normalize(label_orig)) %>%
  mutate ( var_name = val_label_normalize(var_label))


harmonize_eb_trust <- function(x) {
  label_list <- list(
    from = c("^tend\\snot", "^cannot", "^tend\\sto", "^can\\srely",
             "^dk", "^inap", "na"), 
    to = c("not_trust", "not_trust", "trust", "trust",
           "do_not_know", "inap", "inap"), 
    numeric_values = c(0,0,1,1, 99997,99999,99999)
  )
  
  harmonize_values(x, 
                   harmonize_labels = label_list, 
                   na_values = c("do_not_know"=99997,
                                 "declined"=99998,
                                 "inap"=99999)
  )
}

merged_surveys <- merge_waves ( example_surveys, var_harmonization = to_harmonize  )

harmonized <- harmonize_waves(waves = merged_surveys, 
                .f = harmonize_eb_trust,
                status_message = FALSE)

test_that("correct type is returned", {
  expect_true(is.labelled_spss_survey(harmonized$trust_in_institutions_army))
})

test_that("correct range is returned", {
  expect_equal(range(as_numeric(harmonized$trust_in_institutions_european_union), na.rm=TRUE), 
               c(0,1) )
})




