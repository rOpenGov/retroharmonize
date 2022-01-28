examples_dir <- system.file("examples", package = "retroharmonize")
survey_list  <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
example_surveys <- read_surveys(
  file.path( examples_dir, survey_list), 
  export_path = NULL)

## Compare with documentation:

documented_surveys <- metadata_create(example_surveys)
documented_surveys <- documented_surveys[documented_surveys$var_name_orig %in% c( "rowid", "isocntry", "w1", "qd3_4", "qd3_8" , "qd7.4", "qd7.8", "qd6.4", "qd6.8"),]
crosswalk_table    <- crosswalk_table_create ( metadata = documented_surveys )



freedom_vars    <- documented_surveys[grepl("freedom", documented_surveys$var_label_orig),]$var_name_orig
solidarity_vars <- documented_surveys[grepl("solidarity", documented_surveys$var_label_orig),]$var_name_orig


test_that("crosswalk_table_create() creates a schema crosswalk table", {
  expect_true(nrow(crosswalk_table) >= nrow(documented_surveys))
  expect_true(all(names(crosswalk_table) %in% c("id", "filename", "var_name_orig", "var_name_target", 
                                                 "val_numeric_orig", "val_numeric_target", 
                                                 "val_label_orig", "val_label_target", 
                                                 "na_label_orig", "na_label_target",
                                                 "na_numeric_orig", "na_numeric_target",
                                                 "var_label_orig", "var_label_target",
                                                 "class_orig",  "class_target")))
})

expected_total_rows <- sum(vapply(example_surveys, nrow, numeric(1)))

crosswalked_1 <- crosswalk_surveys(
  survey_list = example_surveys, 
  crosswalk_table = crosswalk_table)

test_that("crosswalk_surveys() returns expected survey list", {
  expect_equal(length(crosswalked_1), 3)
  expect_equal(expected_total_rows, 
  sum(
    vapply ( crosswalked_1, nrow, numeric(1))
  )
  )
})


freedom_table <- crosswalk_table[which(crosswalk_table$var_name_target %in% c("rowid", "freedom")),]

h1 <- harmonize_survey_variables(crosswalk_table = freedom_table,
                           subset_name = 'freedom',
                           survey_list = example_surveys )

test_that("harmonize_survey_variables() subsets and renames as expected", {
  expect_true(all(unlist(lapply(h1, function(x) names(x) %in% c("rowid","freedom")))))
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

harmonized_survey_files <- harmonize_survey_variables( 
  survey_list = example_surveys, 
  crosswalk_table = crosswalk_table, 
  export_path = tempdir())

test_that("correct return values for harmonize_survey_variables (files)", {
  expect_equal (length(harmonized_survey_files ),3) 
  expect_true (is.character(harmonized_survey_files))
  expect_true(all(vapply ( file.path(tempdir(),harmonized_survey_files ), fs::file_exists, logical(1))))
})

harmonized_survey_vars <- harmonize_survey_variables ( 
  survey_list = example_surveys, 
  crosswalk_table = crosswalk_table, 
  export_path = NULL)

test_that("correct return values for harmonize_survey_variables (list)", {
  expect_equal (length(harmonized_survey_vars),3) 
  expect_true (is.list(harmonized_survey_vars))
  expect_equal(unlist(lapply (lapply(harmonized_survey_vars, names ), length)), 
               c(5,5,5))
})


test_that("crosswalk_surveys() results in correct harmonization", {
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

test_that("crosswalk_surveys() created the proper NA (missing) label", {
  expect_equal(nrow(crosswalk( survey_list = example_surveys, 
                               crosswalk_table = crosswalk_table, 
                               na_values = c("inap" = 99999 ) )), 
               expected_total_rows)
})

