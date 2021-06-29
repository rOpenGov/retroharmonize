
metadata <- metadata_create (
 survey = read_rds (
          system.file("examples", "ZA7576.rds",
                      package = "retroharmonize")
          ))

names (metadata )
test_codebook <- codebook_create (metadata )

examples_dir <- system.file("examples", package = "retroharmonize")
survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]

example_surveys <- read_surveys(
  file.path( examples_dir, survey_list), 
  save_to_rds = FALSE)

waves_codebook <- codebook_waves_create ( waves = example_surveys )


test_that("correct codebook structure is returned", {
  expect_true(all(names ( test_codebook )[! names ( test_codebook ) %in% names(metadata)] %in% c("entry","val_code_orig","val_label_orig","label_range")))
  expect_true(all(names ( waves_codebook )[! names ( test_codebook ) %in% names(metadata)] %in% c("entry","val_code_orig","val_label_orig","label_range")))
})


test_that("correct codebook contents are returned", {
  expect_true (
    ## all files are present in the codebook
    all (survey_list %in% unique ( waves_codebook$filename))
  )
  expect_true ( 
    # The label range is correctly identified
    all ( c("valid", "missing") %in% unique(waves_codebook$label_range))
    )
  })

test_that("error handling", {
  expect_error ( codebook_create( survey = data.frame()) )
})


