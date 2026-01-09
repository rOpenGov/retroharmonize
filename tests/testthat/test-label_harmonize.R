ln <- label_normalize(
  x = c(
    "Don't know", " TRUST", "DO NOT  TRUST",
    "inap in Q.3", "Not 100%", "TRUST < 50%",
    "TRUST >=90%", "Verify & Check", "TRUST 99%+"
  )
)

vrn <- var_label_normalize(
  c(
    "Q1_Do you trust the national government?",
    " Do you trust the European Commission"
  )
)
vrn

vln <- val_label_normalize(
  c(
    "Q1_Do you trust the national government?",
    " Do you trust the European Commission"
  )
)
vln

c(
  "do_you_trust_the_national_government",
  "do_you_trust_the_european_commission"
)

test_that("normalization works", {
  expect_equal(vrn, c(
    "do_you_trust_the_national_government",
    "do_you_trust_the_european_commission"
  ))
  expect_equal(vln, c(
    "do_you_trust_the_national_government",
    "do_you_trust_the_european_commission"
  ))
  expect_equal(
    ln,
    c(
      "do not know", "trust", "do not trust",
      "inap in q 3", "not 100 pct", "trust lt 50 pct",
      "trust ge 90 pct", "verify and check",
      "trust 99 pct plus"
    )
  )
})
