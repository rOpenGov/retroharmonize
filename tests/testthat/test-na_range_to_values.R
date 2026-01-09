test_that("na_range_to_values harmonizes range-only missing values", {
  var1 <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(
      TRUST = 1,
      `NOT TRUST` = 0,
      `DON'T KNOW` = 8,
      `INAP. HERE` = 9
    ),
    na_range = c(8, 12)
  )
  
  res <- na_range_to_values(var1)
  
  expect_equal(labelled::na_values(res), c(8, 9))
  expect_equal(as_numeric(res), c(1, 0, 1, 1, 0, NA, NA))
})


test_that("explicit na_values outside range trigger warning and adjustment", {
  var2 <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 7, 9),
    labels = c(
      TRUST = 1,
      `NOT TRUST` = 0,
      `DON'T KNOW` = 7,
      `INAP. HERE` = 9
    ),
    na_range = c(8, 12),
    na_values = c(7, 9)
  )
  
  expect_warning(
    res <- na_range_to_values(var2),
    "Inconsistent missing ranges"
  )
  
  expect_equal(labelled::na_values(res), c(7, 9))
  expect_equal(labelled::na_range(res), c(7, 12))
})

test_that("non-SPSS labelled vectors are returned unchanged", {
  var3 <- labelled::labelled(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(TRUST = 1, `NOT TRUST` = 0)
  )
  
  res <- na_range_to_values(var3)
  
  expect_identical(res, var3)
  expect_null(labelled::na_values(res))
})
