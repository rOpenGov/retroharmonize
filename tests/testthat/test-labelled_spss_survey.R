
empty <- labelled_spss_survey(label = "hello")
test_that("label is correct", {
  expect_equal(attr(empty, "label"), "hello")
})

v1 <- labelled_spss_survey (
  x = c(1,0,1,9), 
  labels = c("yes" =1,
             "no" = 0,
             "inap" = 9),
  na_values = 9, 
  label = "Hello")
attributes (v1)
attr ( v1, "label")

x1 <- labelled_spss_survey(
  x = 1:10, 
  labels = c(Good = 1, Bad = 8), 
  na_values = c(9, 10), 
  id = "survey1")

x1

my_x <- 1:10

x2 <- labelled_spss_survey( my_x, 
                            labels  = c(Good = 1, Bad = 8), 
                            na_range = c(9, Inf),
                            label = "Quality Rating", 
                            id = "survey2")

x2
#x3 <- labelled_spss_survey(
#  c("good", "good", "bad", "bad", "inap"), c(Good = "good", Bad = "bad"), 
#  na_values = "inap", 
#  id = "survey1")

test_that("type conversion is correct", {
  expect_equal(as_numeric(x1),c(1:8, NA, NA))
  expect_equal(attr(x2, "survey2_labels"),  c(Good = 1, Bad = 8))
  expect_equal(attr(x1, "survey1_na_values"),  c(9,10))
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
  expect_equal(attr(x1, "id"), "survey1")
  expect_equal(attr(x2, "label"), "Quality Rating")
  expect_equal(attr(x2, "survey2_name"), "my_x")
  expect_equal(attr(x2, "na_range"), c(9, Inf))
  expect_equal(attr(x2, "survey2_na_range"), c(9, Inf))
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

