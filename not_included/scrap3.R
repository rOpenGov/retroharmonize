


example_1 <- tibble::tibble ( a = var1, b = var2, c = var3 )

harmonize_labels <- list ( 
  from = c("^tend\\sto|^trust", "^tend\\snot|not_trust", "^dk", "^inap"), 
  to = c("trust", "not_trust", "do_not_know", "inap"),
  numeric_value = c(1,0,99997, 99999)
  )

na_values <- c("do_not_know", "inap")

harmonize_labels$from

these_labels <- labelled::val_labels( var3 )
these_value_labels <- tolower(names(these_labels))
str <- these_value_labels

na_values_labelled <- harmonize_labels$numeric_value
names(na_values_labelled) <- harmonize_labels$to
na_values_labelled <- na_values_labelled[ which (names(na_values_labelled) %in% na_values)]

for ( s in 1:length(these_value_labels) ) {
  str <- stringi::stri_replace_all_regex(str,
                                  pattern = these_value_labels[s],
                                  replacement = harmonize_labels$to[s],
                                  vectorize_all = F)
}

