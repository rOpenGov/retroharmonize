test_survey <- read_rds (
  file = system.file("examples", "ZA7576.rds",
                     package = "retroharmonize")
)

example_metadata <- metadata_create (
 survey = test_survey
)

example_metadata$n_na_values

q_labels <- length(labelled::val_labels ( test_survey$qd6.12))
q_na     <- length(labelled::na_values ( test_survey$qd6.12))

test_value <- example_metadata[which ( example_metadata$var_name_orig == "qd6.12"), ]
q_labels-q_na


test_that("Correct values are returned", {
  expect_equal(as.character(unlist(example_metadata$na_values[2])), "")
  expect_equal(example_metadata$label_orig[1], "unique identifier in za7576 rds")
  expect_equal(
    length(test_value$na_values), 
    q_na)
  expect_equal(
    test_value$n_cat_labels, 
    q_labels-q_na)
  expect_equal(example_metadata$var_name_orig[1], "rowid")
})
