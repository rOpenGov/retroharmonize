
metadata <- metadata_create (
 survey = read_rds (
          system.file("examples", "ZA7576.rds",
                      package = "retroharmonize")
          ))

names (metadata )
test_codebook <- codebook_create (metadata )

test_that("correct codebook structure is returned", {
  expect_true(all(names ( test_codebook )[! names ( test_codebook ) %in% names(metadata)] %in% c("entry","var_code_orig","var_label_orig","label_range")))
})


test_that("error handling", {
  expect_error ( codebook_create( survey = data.frame()) )
})
