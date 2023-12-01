test_survey <- read_rds (
  file = system.file("examples", "ZA7576.rds",
                     package = "retroharmonize")
)

test_metadata <- metadata_create ( test_survey )
test_metadata <- test_metadata[c(1,7,18),]
ctable_2 <- crosswalk_table_create(test_metadata)
ctable_2$var_name_target  <- ifelse(ctable_2$var_name_orig == "qa14_3", 
                                     "trust_ecb", 
                                      ctable_2$var_name_orig)

subsetted <- subset_surveys  (crosswalk_table = ctable_2, 
                              subset_name = "tested",
                              survey_list =  test_survey,
                              import_path = NULL)


test_that("multiplication works", {
  expect_true(is.survey(subsetted[[1]]))
})
