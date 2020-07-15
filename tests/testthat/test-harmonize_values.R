var1 <- labelled::labelled_spss(
  x = c(1,0,1,1,0,8,9), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0, 
             "DON'T KNOW" = 8, 
             "INAP. HERE" = 9), 
  na_values = c(8,9))

str(var1)

h1 <- harmonize_values (
  x = var1, 
  harmonize_labels = list ( 
    from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"),
    numeric_values = c(1,0,99997, 99999)), 
  na_values = c("do_not_know" = 99997,
                "inap" = 99999), 
  id = "survey_id",
  harmonize_label = "Do you trust the European Union?"
)

test_that("casting works", {
  expect_equal(inherits(h1, "haven_labelled_spss"), TRUE)
  expect_equal(is.numeric(h1), TRUE)
  expect_equal(is.double(h1), TRUE)
})

test_that("attributes work", {
  expect_equal(attr(h1, "label"), "Do you trust the European Union?")
  expect_equal(attr(h1, "id"), "survey_id")
  expect_equal(attr(h1, "survey_id_labels"),
               c("TRUST" = 1, 
                 "NOT TRUST" = 0, 
                 "DON'T KNOW" = 8, 
                 "INAP. HERE" = 9))
  expect_equal(attr(h1, "survey_id_values"),
               c("0" = 0, 
                 "1" = 1, 
                 "8" = 99997, 
                 "9" = 99999))
})

attributes(h1)

test_that("recoding works", {
  expect_equal(vctrs::vec_data(h1), c(1,0,1,1,0,99997,99999))
})

lvar2 <- labelled::labelled_spss(x = c(1,0,7,9), 
                                 labels = c("TRUST" = 1, 
                                            "NOT TRUST" = 0, 
                                            "DON'T KNOW" = 7, 
                                            "INAP. HERE" = 9), 
                                 na_values = c(8,9), 
                                 na_range = c(7,9))

test_that("recasting works", {
  expect_equal(as_numeric(h1), c(1,0,1,1,0,NA,NA))
  expect_equal(levels(as_factor(h1)), c("not_trust", "trust", "do_not_know", "inap"))
  expect_equal(as_character(h1), c("trust", "not_trust",
                                   "trust", "trust", "not_trust", 
                                   "do_not_know", "inap"))
  expect_equal(as_factor(h1),factor ( 
    x = c("trust", "not_trust",
          "trust", "trust", "not_trust", 
          "do_not_know", "inap"), 
    levels = c("not_trust","trust",  "do_not_know", "inap")))
  expect_equal(
    # case when na_range and na_values must be adjusted first
    as_numeric(harmonize_values (
      x = lvar2, 
      harmonize_labels = list ( 
        from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
        to = c("trust", "not_trust", "do_not_know", "inap"),
        numeric_value = c(1,0,99997, 99999)), 
      na_values = c("do_not_know" = 99997,
                    "inap" = 99999), 
        id = "survey_id"
      )), 
    c(1,0,NA,NA)
               )
})

test_that("exception handling works", {
  ## tests validate_harmonize_values
  expect_error (harmonize_values (var1, 
                                  # list not well defined
                                  harmonize_labels = list(
                                    wrong_from = c("a", "b"), 
                                    wrong_to = c("ab", "bc"))
                                  )
  )
  expect_error (harmonize_values (var1, 
                                  # type mismatch
                                  harmonize_labels = c("a", "b"))
  )
})