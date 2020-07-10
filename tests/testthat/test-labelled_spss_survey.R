
library(testthat)
library(retroharmonize)

x1 <- labelled_spss_survey(
  1:10, c(Good = 1, Bad = 8), 
  na_values = c(9, 10), 
  id = "survey1")
  
is.na(x1)

# Print data and metadata 
print(x1)

x2 <- labelled_spss_survey( 1:10, 
 labels  = c(Good = 1, Bad = 8), 
 na_range = c(9, Inf),
 label = "Quality rating", 
 id = "survey1")


is.na(x2)

# Print data and metadata
x2


test_that("NA values are correct", {
  expect_equal(sum(is.na(x1)),2)
})
