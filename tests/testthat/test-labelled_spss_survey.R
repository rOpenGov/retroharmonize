x1 <- labelled_spss_survey(
  1:10, c(Good = 1, Bad = 8), 
  na_values = c(9, 10), 
  id = "survey1")

x2 <- labelled_spss_survey( 1:10, 
 labels  = c(Good = 1, Bad = 8), 
 na_range = c(9, Inf),
 label = "Quality Rating", 
 id = "survey1")


#x3 <- labelled_spss_survey(
#  c("good", "good", "bad", "bad", "inap"), c(Good = "good", Bad = "bad"), 
#  na_values = "inap", 
#  id = "survey1")


test_that("type conversion is correct", {
  expect_equal(as_numeric(x1),c(1:8, NA, NA))
})

test_that("NA values are correct", {
  expect_equal(sum(is.na(x1)),2)
  expect_equal(levels(as_factor(x1)),c("Good", 2:7, "Bad", 9:10))
  expect_equal(as_character(x1), c("Good", 2:7, "Bad", 9:10))
})

test_that("errors work", {
  expect_error(sum(as_factor(x1)))
})

test_that("attributes are present", {
  expect_equal(attr(x2, "id"), "survey1")
  expect_equal(attr(x2, "label"), "Quality Rating")
})

test_that("arithmetic methods work", {
  expect_equal(sum(x1, na.rm=TRUE),
               sum(1:8))
  expect_equal(mean(x1, na.rm=TRUE),
               mean(1:8))
  expect_equal(median(x1),
               median(1:8))
  expect_equal(quantile(x1,0.25),
               quantile(1:8,0.25)) 
  expect_equal(weighted.mean (x1, c(2, rep(1,9))),
               weighted.mean (1:8, c(2, rep(1,7)))
  )
})

verify_output("retroh_int_shaft.txt", {
  pillar::pillar_shaft(tibble(v1=x1))
})

verify_output("retroh_int.txt", {
  pillar::pillar(x1)
})

