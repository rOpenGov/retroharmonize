load_example_surveys <- function() {
  examples_dir <- system.file("examples", package = "retroharmonize")
  files <- dir(examples_dir, pattern = "\\.rds$")
  read_surveys(
    file.path(examples_dir, files),
    export_path = NULL
  )
}

make_base_crosswalk <- function() {
  surveys <- load_example_surveys()
  
  metadata <- metadata_create(surveys)
  metadata <- metadata[
    metadata$var_name_orig %in% c(
      "rowid", "isocntry", "w1",
      "qd3_4", "qd3_8",
      "qd7.4", "qd7.8",
      "qd6.4", "qd6.8"
    ),
  ]
  
  list(
    surveys = surveys,
    metadata = metadata,
    crosswalk = crosswalk_table_create(metadata)
  )
}

test_that("crosswalk_table_create() creates a valid crosswalk schema", {
  obj <- make_base_crosswalk()
  cw  <- obj$crosswalk
  
  expect_s3_class(cw, "data.frame")
  
  expect_true(
    all(
      c(
        "id", "filename",
        "var_name_orig", "var_name_target",
        "val_numeric_orig", "val_numeric_target",
        "val_label_orig", "val_label_target",
        "na_label_orig", "na_label_target",
        "na_numeric_orig", "na_numeric_target",
        "var_label_orig", "var_label_target",
        "class_orig", "class_target"
      ) %in% names(cw)
    )
  )
  
  expect_gte(nrow(cw), nrow(obj$metadata))
})

test_that("is.crosswalk_table() validates uniqueness of target variables", {
  obj <- make_base_crosswalk()
  
  expect_silent(is.crosswalk_table(obj$crosswalk))
  
  bad <- obj$crosswalk
  bad$var_name_target[bad$id == bad$id[1]][1:2] <- "dup"
  
  expect_error(
    is.crosswalk_table(bad),
    "non-unique target variables"
  )
})

test_that("harmonize_survey_variables() subsets and renames variables", {
  obj <- make_base_crosswalk()
  
  freedom_table <- obj$crosswalk[
    obj$crosswalk$var_name_target %in% c("rowid", "freedom"),
  ]
  
  out <- harmonize_survey_variables(
    survey_list = obj$surveys,
    crosswalk_table = freedom_table,
    subset_name = "freedom"
  )
  
  expect_length(out, length(obj$surveys))
  
  for (s in out) {
    expect_true(all(names(s) %in% c("rowid", "freedom")))
  }
})

test_that("harmonize_survey_variables() writes files when export_path is set", {
  obj <- make_base_crosswalk()
  
  files <- harmonize_survey_variables(
    survey_list = obj$surveys,
    crosswalk_table = obj$crosswalk,
    export_path = tempdir()
  )
  
  expect_type(files, "character")
  expect_length(files, length(obj$surveys))
  
  expect_true(
    all(fs::file_exists(file.path(tempdir(), files)))
  )
})

test_that("crosswalk_surveys() returns a list of harmonized surveys", {
  obj <- make_base_crosswalk()
  
  out <- crosswalk_surveys(
    survey_list = obj$surveys,
    crosswalk_table = obj$crosswalk
  )
  
  expect_type(out, "list")
  expect_length(out, length(obj$surveys))
  
  expected_rows <- sum(vapply(obj$surveys, nrow, numeric(1)))
  actual_rows   <- sum(vapply(out, nrow, numeric(1)))
  
  expect_equal(actual_rows, expected_rows)
})

test_that("crosswalk_surveys() harmonizes names and labels correctly", {
  obj <- make_base_crosswalk()
  cw  <- obj$crosswalk
  
  freedom_vars <- obj$metadata$var_name_orig[
    grepl("freedom", obj$metadata$var_label_orig)
  ]
  
  solidarity_vars <- obj$metadata$var_name_orig[
    grepl("solidarity", obj$metadata$var_label_orig)
  ]
  
  cw$var_name_target <- ifelse(
    cw$var_name_orig %in% freedom_vars, "freedom",
    ifelse(
      cw$var_name_orig %in% solidarity_vars,
      "solidarity",
      cw$var_name_orig
    )
  )
  
  cw$val_label_target <- ifelse(
    grepl("Mentioned|freedom|Solidarity", cw$val_label_orig),
    "mentioned",
    ifelse(
      cw$val_label_orig == "Not mentioned",
      "not_mentioned",
      ifelse(grepl("Inap", cw$val_label_orig), "inap", NA)
    )
  )
  
  cw$val_numeric_target <- ifelse(
    cw$val_label_target == "inap",
    99999,
    cw$val_numeric_orig
  )
  
  out <- crosswalk_surveys(
    survey_list = obj$surveys,
    crosswalk_table = cw,
    na_values = c("inap" = 99999)
  )
  
  expect_true(
    all(names(out[[1]]) ==
          c("id", "rowid", "isocntry", "freedom", "solidarity", "w1"))
  )
  
  all_values <- unique(unlist(lapply(out, function(x) as_character(x$freedom))))
  
  expect_true(
    all(all_values %in% c("mentioned", "not_mentioned", "inap"))
  )
})

test_that("crosswalk() returns a combined data frame", {
  obj <- make_base_crosswalk()
  
  out <- crosswalk(
    survey_list = obj$surveys,
    crosswalk_table = obj$crosswalk
  )
  
  expect_s3_class(out, "data.frame")
  
  expected_rows <- sum(vapply(obj$surveys, nrow, numeric(1)))
  expect_equal(nrow(out), expected_rows)
})

