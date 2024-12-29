
## Scenarios with different labels ----------------------------
test_that("subsetting workds", {
  v1 <- labelled_spss_survey (
    x = c(1,0,1,9), 
    labels = c("yes"   = 1,
               "no"   =  0,
               "inap" = 9),
    na_values = 9, 
    label = "My survey item", 
    id = "test"
  )
  expect_equal(as_numeric(v1[3:4]), c(1,NA))
})


test_that("coercion exception handling works", {
  v1 <- labelled_spss_survey (
    x = c(1,0,1,9), 
    labels = c("yes"   = 1,
               "no"   =  0,
               "inap" = 9),
    na_values = 9, 
    label = "My survey item", 
    id = "test"
  )
  v2 <- labelled_spss_survey (
    x = c(0,0,9,1,1), 
    labels = c("yes" =1,
               "no" = 0,
               "inap" = 9),
    na_values = 9, 
    label = "Other survey item", 
    id = "test2"
  )
  v3 <- labelled_spss_survey (
    x = c(0,0,9,1,1), 
    labels = c("ja" =1,
               "nee" = 0,
               "vermist" = 9),
    na_values = 9, 
    label = "Other survey item", 
    id = "test2"
  )
  v4 <- labelled_spss_survey (
    x = c(0,0,9,1,1), 
    labels = c("ja" =1,
               "nee" = 0,
               "vermist" = 9),
    na_values = 8, 
    label = "Other survey item", 
    id = "test2"
  )
  v5 <- labelled_spss_survey (
    x = c(0,0,9,1,1), 
    labels = c("ja" =1,
               "nee" = 0,
               "ontbekend" = 9),
    na_values = 8, 
    label = "Other survey item", 
    id = "test2"
  )
  expect_error(vec_c(v1, v3))
  expect_error(vec_c(v3, v4))
  expect_error(vec_c(v5, v4))
})


test_that("coercion works", {
  v1 <- labelled_spss_survey (
    x = c(1,0,1,9), 
    labels = c("yes"   = 1,
               "no"   =  0,
               "inap" = 9),
    na_values = 9, 
    label = "My survey item", 
    id = "test"
  )
  expect_true(is.numeric(c(pi, v1)))
  expect_true(is.double(c(v1, 1)))
  expect_error(vctrs::vec_c("hello", v1))
  expect_equal(is.na(v1), c(F,F,F,T))
})

test_that("type conversion is correct", {
  my_x <- 1:10
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  x2 <- labelled_spss_survey( my_x, 
                              labels  = c(Good = 1, Bad = 8), 
                              na_range = c(9, Inf),
                              label = "Quality Rating", 
                              id = "survey2")
  expect_equal(as_numeric(x1),c(1:8, NA, NA))
  expect_equal(attr(x2, "survey2_labels"),  c(Good = 1, Bad = 8))
  expect_equal(attr(x1, "survey1_na_values"),  c(9,10))
})

test_that("NA values are correct", {
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  expect_equal(sum(is.na(x1)),2)
  expect_equal(levels(as_factor(x1)),c("Good", 2:7, "Bad", 9:10))
  expect_equal(retroharmonize::as_character(x= x1), c("Good", 2:7, "Bad", 9:10))
})

test_that("errors work", {
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  
  expect_error(sum(as_factor(x1)))
})

test_that("attributes are present", {
  my_x <- 1:10
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  x2 <- labelled_spss_survey( my_x, 
                              labels  = c(Good = 1, Bad = 8), 
                              na_range = c(9, Inf),
                              label = "Quality Rating", 
                              id = "survey2")
  expect_equal(attr(x1, "id"), "survey1")
  expect_equal(attr(x2, "label"), "Quality Rating")
  expect_equal(attr(x2, "survey2_name"), "my_x")
  expect_equal(attr(x2, "na_range"), c(9, Inf))
  expect_equal(attr(x2, "survey2_na_range"), c(9, Inf))
})

test_that("arithmetic methods work", {
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  expect_equal(sum(x1, na.rm=TRUE),
               sum(1:8))
  expect_equal(mean(x1, na.rm=TRUE),
               mean(1:8))
  expect_equal(median(x1),
               median(1:8))
  expect_equal(quantile(x1,0.25),
               quantile(1:8,0.25)) 
  expect_equal(weighted.mean (x1, w = c(2, rep(1,9))),
               weighted.mean (1:8, w = c(2, rep(1,7)))
  )
})

verify_output("retroh_int_shaft.txt", {
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  pillar::pillar_shaft(tibble(v1=x1))
})

verify_output("retroh_int.txt", {
  x1 <- labelled_spss_survey(
    x = 1:10, 
    labels = c(Good = 1, Bad = 8), 
    na_values = c(9, 10), 
    id = "survey1")
  pillar::pillar(x1)
})

