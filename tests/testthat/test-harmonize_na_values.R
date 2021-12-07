test_locally <- function() {
  
  examples_dir <- system.file( "examples", package = "retroharmonize")
  
  test_read <- read_rds ( 
    file.path(examples_dir, "ZA7576.rds"),
    id = "ZA7576", 
    doi = "test_doi")
  
  tested <- harmonize_na_values( df = test_read)
  
  inherits(tested, "survey")
}


test_that("example works for harmonize_na_values", {
  skip_on_cran()
  expect_true(test_locally())
})
