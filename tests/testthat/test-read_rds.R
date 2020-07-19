path <- system.file("examples", "ZA7576.rds", package = "retroharmonize")
test_read <- read_rds ( path, id = "ZA7576", 
                        doi = "test_doi")

attributes( test_read )

test_that("attributes work", {
  expect_equal(attr(test_read, "id"), "ZA7576")
  expect_equal(attr(test_read, "filename"), "ZA7576.rds")
  expect_equal(attr(test_read, "doi"), "test_doi")
  expect_true(inherits(test_read, "survey"))
})



