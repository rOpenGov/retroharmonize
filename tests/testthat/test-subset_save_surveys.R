
testing_subsetting <- function() {
  test_survey <- read_rds (
    file = system.file("examples", "ZA7576.rds",
                       package = "retroharmonize")
  )
  
  test_metadata <- metadata_create ( test_survey )
  test_metadata <- test_metadata[c(18:37),]
  test_metadata$var_name  <- var_label_normalize (test_metadata$var_name_orig)
  test_metadata$var_label <- test_metadata$label_orig
  
  saveRDS(test_survey, file.path(tempdir(), 
                                 "ZA7576.rds"), 
          version = 2)
  
  subset_save_surveys  ( var_harmonization = test_metadata, 
                         selection_name = "tested",
                         import_path = tempdir(), 
                         export_path = tempdir())
  
  file.exists ( file.path(tempdir(), "ZA7576_tested.rds"))
}
 
test_that("saving and subsetting", {
  skip_on_cran()
  expect_true(testing_subsetting())
  expect_message(testing_subsetting())
})
