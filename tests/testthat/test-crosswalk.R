examples_dir <- system.file("examples", package = "retroharmonize")
survey_list  <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
example_surveys <- read_surveys(
  file.path( examples_dir, survey_list), 
  save_to_rds = FALSE)

## Compare with documentation:

documented_surveys <- metadata_surveys_create(example_surveys)
documented_surveys <- documented_surveys[documented_surveys$var_name_orig %in% c( "rowid", "isocntry", "w1", "qd3_4", "qd3_8" , "qd7.4", "qd7.8", "qd6.4", "qd6.8"),]
crosswalk_table    <- crosswalk_table_create ( metadata = documented_surveys )

freedom_vars <- documented_surveys[grepl("freedom", documented_surveys$label_orig),]$var_name_orig
solidarity_vars <- documented_surveys[grepl("solidarity", documented_surveys$label_orig),]$var_name_orig

test_that("crosswalk_table_create", {
  expect_true(nrow(crosswalk_table) >= nrow(documented_surveys))
  expect_equal(names(crosswalk_table),c("id", "filename", "var_name_orig", "var_name_target", 
                                        "val_numeric_orig", "val_numeric_target", 
                                        "val_label_orig", "val_label_target"))
})

expected_total_rows <- sum(vapply ( example_surveys, nrow, numeric(1) ))

crosswalked_1 <- crosswalk_surveys(survey_list = example_surveys, 
                                 crosswalk_table = crosswalk_table)

test_that("crosswalk_surveys", {
  expect_equal(length(crosswalked_1), 3)
  expect_equal(expected_total_rows, 
  sum(
    vapply ( crosswalked_1, nrow, numeric(1))
  )
  )
})

crosswalk_table$val_label_target <- ifelse (
  test = grepl ( "freedom|Solidarity|Mentioned", crosswalk_table$val_label_orig), 
  yes = "mentioned", 
  no = ifelse ( 
    test = crosswalk_table$val_label_orig == "Not mentioned", 
    yes = "not_mentioned", 
    no  = ifelse(test=grepl("Inap", crosswalk_table$val_label_orig), 
                  yes = "inap", 
                  no = NA)
    )
)

crosswalk_table$var_name_target <- ifelse (
  test = crosswalk_table$var_name_orig %in% freedom_vars, 
  yes  = "freedom", 
  no   =   ifelse ( 
    test = crosswalk_table$var_name_orig %in% solidarity_vars, 
    yes = "solidarity", 
    no  = crosswalk_table$var_name_orig
  ))

crosswalk_table$val_numeric_target = ifelse ( crosswalk_table$val_label_target == "inap", 
                                              99999, crosswalk_table$val_numeric_orig)

crosswalked_2 <-  crosswalk_surveys(
  survey_list = example_surveys, 
  crosswalk_table = crosswalk_table, 
  na_values = c("inap" = 99999 ) )


test_that("crosswalk_surveys_correct_harmonization", {
  expect_equal(length(crosswalk_surveys(survey_list = example_surveys, 
                                        crosswalk_table = crosswalk_table)), 3)
  expect_equal(sum(
    vapply ( example_surveys, nrow, numeric(1) )
  ), 
  sum(
    vapply ( crosswalked_2, nrow, numeric(1))
  )
  )
  expect_true (
    ## Correct var_name harmonization
    all(names (crosswalked_2[[1]]) == c("id", "rowid", "isocntry", "freedom", "solidarity","w1"  ) ))
  expect_true ( 
    all (unique(unlist(lapply ( crosswalked_2, function(x) unique(as_character(x$freedom)) ))) %in% c("not_mentioned", "mentioned", "inap")))
})

test_that("crosswalk_surveys_correct_harmonization", {
  expect_equal(nrow(crosswalk( survey_list = example_surveys, 
                               crosswalk_table = crosswalk_table, 
                               na_values = c("inap" = 99999 ) )), expected_total_rows)
})

new_documentation <- metadata_surveys_create ( crosswalked_2 )

