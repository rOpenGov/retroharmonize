library(haven)
var1 <- labelled::labelled_spss(
  x = c(1, 0, 1, 1, 0, 8, 9),
  labels = c(
    "TRUST" = 1,
    "NOT TRUST" = 0,
    "DON'T KNOW" = 8,
    "INAP. HERE" = 9
  ),
  na_values = c(8, 9)
)

var2 <- labelled::labelled_spss(
  x = c(1, 6, 2, 9, 1, 1, 2),
  labels = c(
    "Tend to trust" = 1,
    "Tend not to trust" = 2,
    "DK" = 8,
    "Inap" = 9
  ),
  na_values = c(6, 9)
)

var3 <- labelled::labelled(
  x = c(1, 6, 2, 9, 1, 1, 2),
  labels = c(
    "Tend to trust" = 1,
    "Tend not to trust" = 2,
    "DK" = 6
  )
)

usethis::use_data(var1, var2, var3, internal = TRUE, overwrite = TRUE)
