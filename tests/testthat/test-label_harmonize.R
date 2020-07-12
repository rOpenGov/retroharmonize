ln <- label_normalize (
x = c("Don't know", " TRUST", "DO NOT  TRUST", 
  "inap in Q.3", "Not 100%", "TRUST < 50%", 
  "TRUST >=90%", "Verify & Check", "TRUST 99%+")
)

vrn <- var_label_normalize ( 
      c("Q1_Do you trust the national government?", 
        " Do you trust the European Commission")
        )

vln <-  val_label_normalize ( 
      c("Q1_Do you trust the national government?", 
        " Do you trust the European Commission")
        )

c( "do_you_trust_the_national_government",
   "do_you_trust_the_european_commission")

test_that("normalization works", {
  expect_equal(vrn, c("q_1_do_you_trust_the_national_government",
                      "do_you_trust_the_european_commission"))
  expect_equal(vln, c("do_you_trust_the_national_government",
                      "do_you_trust_the_european_commission"))
  expect_equal(ln, 
               c("do_not_know","trust","do_not_trust",     
                 "inap_in_q_3", "not_100_pct","trust_lt_50_pct",
                 "trust_ge_90_pct", "verify_and_check" ,
                 "trust_99_pct_plus"))
})
