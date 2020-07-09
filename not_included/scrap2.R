
v1 <- labelled_spss_survey (c(1,0,1,9), labels = c("yes" =1,
                                             "no" = 0,
                                             "inap" = 9),
                      na_values = 9)


v2 <- labelled_spss_survey (c(1,1,0,8), labels = c("yes" =1,
                                             "no" = 0,
                                             "declined" = 8),
                      na_values = 8)

v3 <- labelled_spss_survey (c(1,1,0,8), labels = c("yes" =2,
                                                   "no" = 1,
                                                   "declined" = 8),
                            na_values = 8)



a <- tibble::tibble ( v1 = v1, v2 = v2, v3 = v3)
b <- tibble::tibble ( v1 = v2, v2 = v1)

harmonized_labels <- c("yes" =1,
                       "no" = 0)

harmonize_labels <- function(x, id = "id1") {

  sapply ( a, labelled::val_labels )
  na_values <- sapply ( a, labelled::na_values )
  na_range <- unique ( as.numeric(na_values))

  code_table <- tibble (
    harmonized = harmonized_labels )


}
