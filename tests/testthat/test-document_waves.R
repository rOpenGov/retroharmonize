examples_dir <- system.file( "examples", package = "retroharmonize")

my_rds_files <- dir( examples_dir)[grepl(".rds", 
                                         dir(examples_dir))]

example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))


documented <- document_surveys ( example_surveys )

test_that("correct attributes are returned", {
  expect_equal(documented$id, c("ZA5913", "ZA6863", "ZA7576"))
})
