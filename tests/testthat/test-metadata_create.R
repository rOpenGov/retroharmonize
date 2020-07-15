test_survey <- read_rds (
  file = system.file("examples", "ZA7576.rds",
                     package = "retroharmonize")
)

example_metadata <- metadata_create (
 survey = test_survey
)

q_labels <- length(labelled::val_labels ( test_survey$qd6.12))
q_na     <- length(labelled::na_values ( test_survey$qd6.12))

test_that("Correct values are returned", {
  expect_equal(is.null(unlist(example_metadata$na_levels[2])), TRUE)
  expect_equal(example_metadata$label_orig[1], "unique identifier in za7576 rds")
  expect_equal(
    as.numeric(example_metadata$n_na_values[which ( names(test_survey) == "qd6.12")]), 
    q_na)
  expect_equal(
    as.numeric(example_metadata$n_cat_values[which ( names(test_survey) == "qd6.12")]), 
    q_labels-q_na)
  expect_equal(example_metadata$var_name_orig[1], "rowid")
})
