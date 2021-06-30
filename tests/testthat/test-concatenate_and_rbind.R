
var1 <- labelled::labelled_spss(
  x = c(1,0,1,1,0,8,9), 
  labels = c("TRUST" = 1, 
             "NOT TRUST" = 0, 
             "DON'T KNOW" = 8, 
             "INAP. HERE" = 9), 
  na_values = c(8,9))

var2 <- labelled::labelled_spss(
  x = c(2,2,8,9,1,1 ), 
  labels = c("Tend to trust" = 1, 
             "Tend not to trust" = 2, 
             "DK" = 8, 
             "Inap" = 9), 
  na_values = c(8,9))


h1 <- harmonize_values (
  x = var1, 
  harmonize_label = "Do you trust the European Union?",
  harmonize_labels = list ( 
    from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"),
    numeric_values = c(1,0,99997, 99999)), 
  na_values = c("do_not_know" = 99997,
                "inap" = 99999), 
  id = "survey1",

)

h2 <- harmonize_values (
  x = var2, 
  harmonize_label = "Do you trust the European Union?",
  harmonize_labels = list ( 
    from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"), 
    to = c("trust", "not_trust", "do_not_know", "inap"),
    numeric_values = c(1,0,99997, 99999)), 
  na_values = c("do_not_know" = 99997,
                "inap" = 99999), 
  id = "survey2"
)

h3 <- concatenate(h1, h2)

test_that("correct values are returned", {
  expect_true (inherits(h3, "retroharmonize_labelled_spss_survey"))
  expect_equal(retroharmonize::as_numeric(h3), c(1,0,1,1,0,NA,NA, 
                                 0,0,NA,NA, 1,1))
  expect_equal(retroharmonize::as_character(h3), c("trust","not_trust","trust", "trust","not_trust","do_not_know","inap", 
                                   "not_trust","not_trust","do_not_know","inap", "trust","trust"))
  expect_equal(levels(retroharmonize::as_factor(h3)), c("not_trust", "trust", "do_not_know", "inap"))
  
})


a <- tibble::tibble ( rowid = paste0("survey1", 1:length(h1)),
                      hvar = h1, 
                      w = runif(n = length(h1), 0,1))
b <- tibble::tibble ( rowid = paste0("survey2", 1:length(h2)),
                      hvar  = h2, 
                      w = runif(n = length(h2), 0,1))

c <- dplyr::bind_rows(a,b)

d <- vctrs::vec_rbind(a,b)

x = a$hvar
y = b$hvar

test_that("correct values are returned", {
  expect_equal(attr(d$hvar, "survey2_labels"),  c("Tend to trust" = 1, 
                                                  "Tend not to trust" = 2, 
                                                  "DK" = 8, 
                                                  "Inap" = 9))
  expect_equal(attr(c$hvar, "survey2_labels"),  c("Tend to trust" = 1, 
                                                  "Tend not to trust" = 2, 
                                                  "DK" = 8, 
                                                  "Inap" = 9))
  expect_equal(attr(c$hvar, "survey1_labels"), c("TRUST" = 1, 
                                                 "NOT TRUST" = 0, 
                                                 "DON'T KNOW" = 8, 
                                                 "INAP. HERE" = 9)
  )
})

