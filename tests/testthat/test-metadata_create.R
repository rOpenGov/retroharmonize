test_survey <- read_rds (
  file = system.file("examples", "ZA7576.rds",
                     package = "retroharmonize")
)

examples_dir <- system.file( "examples", package = "retroharmonize")

my_rds_files <- dir( examples_dir)[grepl(".rds", 
                                         dir(examples_dir))]

example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))

test_that("Only surveys are accepted", {
  expect_error(metadata_create ( data.frame ( a = 1:2, 
                                        b = c("b", "C"))))
  })

example_metadata <- metadata_create (
 survey = test_survey
)

q_labels <- length(labelled::val_labels ( test_survey$qd6.12))
q_na     <- length(labelled::na_values  ( test_survey$qd6.12))

test_value <- example_metadata[which ( example_metadata$var_name_orig == "qd6.12"), ]
q_labels-q_na

test_value2 <- example_metadata[which ( example_metadata$var_name_orig == "qg8"), ]

test_that("Correct values are returned", {
  expect_true(example_metadata$var_name_orig[1] == "rowid")
  expect_equal(unique(example_metadata$filename), "ZA7576.rds")
  expect_equal(as.character(unlist(example_metadata$na_labels[2])), NA_character_)
  expect_equal(example_metadata$label_orig[1], "unique identifier in za7576")
  expect_equal(
    length(test_value$na_labels), 
    q_na)
  expect_equal(
    test_value$n_valid_labels, 
    q_labels-q_na)
  expect_equal(example_metadata$var_name_orig[1], "rowid")
  expect_equal(c(
    length(test_value2$labels[[1]]), 
    length(test_value2$valid_labels[[1]]),
    length(test_value2$na_labels[[1]])
  ), c(8,6,2))
})


metadata_waves <- metadata_waves_create( example_surveys )


test_that("Correct values are returned from waves", {
  expect_true(metadata_waves$var_name_orig[1] == "rowid") 
  })
