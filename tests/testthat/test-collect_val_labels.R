test_survey <- retroharmonize::read_rds (
  file = system.file("examples", "ZA7576.rds",
                     package = "retroharmonize"), 
  id = "test"
)

example_metadata <- metadata_create (
  survey = test_survey
)

trust_media_metadata <- example_metadata[27,]

test_that("val label collection works", {
  expect_equal(collect_val_labels(metadata = trust_media_metadata ), 
               c("Tend to trust", "Tend not to trust", "DK"))
  expect_equal(collect_na_labels(metadata = trust_media_metadata ), 
               c("Inap. (CY-TCC in isocntry)"))
})
