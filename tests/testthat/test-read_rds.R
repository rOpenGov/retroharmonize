path <- here("inst", "examples")
test_read <- read_rds ( file.path(path, "ZA7576.rds"),
                        id = "ZA7576", 
                        doi = "test_doi")

attributes( test_read )

test_that("attributes work", {
  expect_equal(attr(test_read, "id"), "ZA7576")
  expect_equal(attr(test_read, "filename"), "ZA7576.rds")
  expect_equal(attr(test_read, "doi"), "test_doi")
})



