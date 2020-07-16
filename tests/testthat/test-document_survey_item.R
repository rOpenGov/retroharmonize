
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

documented <- document_survey(h3)


test_that("attributes are present", {
  expect_equal(documented$history_var_name, c("name" = "h3",
                                              "survey1_name" = "var1",
                                              "survey2_name" = "var2"))
})
