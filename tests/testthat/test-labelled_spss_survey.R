library(testthat)
library(retroharmonize)

x1 <- labelled_spss_survey(
  1:10, c(Good = 1, Bad = 8), 
  na_values = c(9, 10), 
  id = "survey1")
  
is.na(x1)
to_character(x1)
print(x1)

x2 <- labelled_spss_survey( 1:10, 
 labels  = c(Good = 1, Bad = 8), 
 na_range = c(9, Inf),
 label = "Quality Rating", 
 id = "survey1")


test_that("type conversion is correct", {
  expect_equal(as_numeric(x1),c(1:8, NA, NA))
})

test_that("NA values are correct", {
  expect_equal(sum(is.na(x1)),2)
  expect_equal(levels(as_factor(x1)),c("Good", 2:7, "Bad", 9:10))
  expect_equal(as_character(x1), c("Good", 2:7, "Bad", 9:10))
  expect_equal(sum(x1, na.rm=TRUE),sum(1:8))
  expect_equal(sum(x1, na.rm=FALSE),NA_real_)
})

test_that("errors work", {
  expect_error(sum(as_factor(x1)))
})

test_that("attributes are present", {
  expect_equal(attr(x2, "id"), "survey1")
  expect_equal(attr(x2, "label"), "Quality Rating")
})


