var1 <- labelled::labelled_spss(
  x = c(1,0,1,1,0,8,9), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0, 
             "DON'T KNOW" = 8, 
             "INAP. HERE" = 9), 
  na_range = c(8,12))

var2 <- labelled::labelled_spss(
  x = c(1,0,1,1,0,7,9), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0, 
             "DON'T KNOW" = 7, 
             "INAP. HERE" = 9), 
  na_range = c(8,12), 
  na_values = c(7,9))

var3 <- labelled::labelled(
  x = c(1,0,1,1,0,8,9), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0))

var4 <- labelled::labelled_spss(
  x = c(1,0,8.5), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0), 
  na_range = c(8,9))

test_that("harmonization_works", {
  expect_equal(attr(na_range_to_values (x=var1), "na_values"),c(8,9))
  expect_equal(attr(na_range_to_values(x=var2), "na_values"),c(7,9))
  expect_equal(attr(na_range_to_values(x=var2), "na_range"), c(7,12))
  expect_equal(as_numeric(na_range_to_values(var4)), c(1,0,NA_real_))
  expect_equal(attr(na_range_to_values(x=var3), "na_values"),NULL)
})

test_that("warning works", {
  expect_warning(na_range_to_values(x=var2))
})

