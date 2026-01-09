test_that("harmonize_values casts labelled_spss correctly", {
  
  var_1 <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(
      "TRUST" = 1,
      "NOT TRUST" = 0,
      "DON'T KNOW" = 8,
      "INAP. HERE" = 9
    ),
    na_values = c(8, 9)
  )
  
  h1 <- harmonize_values(
    x = var_1,
    harmonize_labels = list(
      from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"),
      to = c("trust", "not_trust", "do_not_know", "inap"),
      numeric_values = c(1, 0, 99997, 99999)
    ),
    na_values = c(
      "do_not_know" = 99997,
      "declined"    = 99998,
      "inap"        = 99999
    ),
    id = "survey_id",
    harmonize_label = "Do you trust the European Union?"
  )
  
  expect_s3_class(h1, "haven_labelled_spss")
  expect_true(is.numeric(h1))
  expect_true(is.double(h1))
})


test_that("harmonize_values stores metadata attributes", {
  
  var_1 <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(
      "TRUST" = 1,
      "NOT TRUST" = 0,
      "DON'T KNOW" = 8,
      "INAP. HERE" = 9
    ),
    na_values = c(8, 9)
  )
  
  h1 <- harmonize_values(
    x = var_1,
    harmonize_labels = list(
      from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"),
      to = c("trust", "not_trust", "do_not_know", "inap"),
      numeric_values = c(1, 0, 99997, 99999)
    ),
    na_values = c(
      "do_not_know" = 99997,
      "declined"    = 99998,
      "inap"        = 99999
    ),
    id = "survey_id",
    harmonize_label = "Do you trust the European Union?"
  )
  
  expect_equal(attr(h1, "label"), "Do you trust the European Union?")
  expect_equal(attr(h1, "id"), "survey_id")
  
  expect_equal(
    attr(h1, "survey_id_labels"),
    c(
      "TRUST" = 1,
      "NOT TRUST" = 0,
      "DON'T KNOW" = 8,
      "INAP. HERE" = 9
    )
  )
  
  expect_equal(
    attr(h1, "survey_id_values"),
    c("0" = 0, "1" = 1, "8" = 99997, "9" = 99999)
  )
})


test_that("harmonize_values recodes values correctly", {
  
  var_1 <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(
      "TRUST" = 1,
      "NOT TRUST" = 0,
      "DON'T KNOW" = 8,
      "INAP. HERE" = 9
    ),
    na_values = c(8, 9)
  )
  
  h1 <- harmonize_values(
    x = var_1,
    harmonize_labels = list(
      from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"),
      to = c("trust", "not_trust", "do_not_know", "inap"),
      numeric_values = c(1, 0, 99997, 99999)
    ),
    na_values = c(
      "do_not_know" = 99997,
      "declined"    = 99998,
      "inap"        = 99999
    )
  )
  
  expect_equal(
    as.vector(h1),
    c(1, 0, 1, 1, 0, 99997, 99999)
  )
})


test_that("harmonize_values recasting helpers work", {
  
  var_1 <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(
      "TRUST" = 1,
      "NOT TRUST" = 0,
      "DON'T KNOW" = 8,
      "INAP. HERE" = 9
    ),
    na_values = c(8, 9)
  )
  
  h1 <- harmonize_values(
    x = var_1,
    harmonize_labels = list(
      from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"),
      to = c("trust", "not_trust", "do_not_know", "inap"),
      numeric_values = c(1, 0, 99997, 99999)
    ),
    na_values = c(
      "do_not_know" = 99997,
      "declined"    = 99998,
      "inap"        = 99999
    )
  )
  
  expect_equal(as_numeric(h1), c(1, 0, 1, 1, 0, NA, NA))
  expect_equal(
    levels(as_factor(h1)),
    c("not_trust", "trust", "do_not_know", "declined", "inap")
  )
  expect_equal(
    as_character(h1),
    c("trust", "not_trust", "trust", "trust",
      "not_trust", "do_not_know", "inap")
  )
})


test_that("harmonize_values handles invalid input gracefully", {
  
  var_1 <- labelled::labelled_spss(
    x = c(1, 0, 1),
    labels = c("TRUST" = 1, "NOT TRUST" = 0),
    na_values = 8
  )
  
  expect_error(
    harmonize_values(
      var_1,
      harmonize_labels = list(wrong_from = c("a"), wrong_to = c("b"))
    )
  )
  
  expect_error(
    harmonize_values(var_1, harmonize_labels = c("a", "b"))
  )
})


test_that("harmonize_values works with overlapping labels", {
  
  mc_var <- labelled::labelled_spss(
    x = c(1, 0, 1, 1, 0, 8, 9),
    labels = c(
      "Mentioned" = 1,
      "Not mentioned" = 0,
      "DON'T KNOW" = 8,
      "INAP. HERE" = 9
    ),
    na_values = c(8, 9)
  )
  
  h_mc <- harmonize_values(
    x = mc_var,
    harmonize_labels = list(
      from = c("^mentioned", "^not mentioned", "^dk|^don", "^inap"),
      to = c("mentioned", "not_mentioned", "do_not_know", "inap"),
      numeric_values = c(1, 0, 99997, 99999)
    )
  )
  
  expect_setequal(
    unique(as_character(h_mc)),
    c("mentioned", "not_mentioned", "do_not_know", "inap")
  )
})
