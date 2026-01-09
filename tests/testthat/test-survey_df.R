test_that("survey_df() constructs a survey from a data frame", {
  
  df <- data.frame(
    rowid = 1:6,
    observations = runif(6)
  )
  
  survey <- survey_df(
    x = df,
    identifier = "example",
    filename = "no_file"
  )
  
  expect_s3_class(survey, "survey_df")
  expect_true(is.data.frame(survey))
  expect_true(is.numeric(survey$observations))
  expect_equal(attr(survey, "id"), "example")
  expect_equal(attr(survey, "filename"), "no_file")
})

test_that("survey_df() creates default dataset metadata", {
  
  survey <- survey_df(
    x = data.frame(a = 1:3),
    identifier = "example",
    filename = "file.csv"
  )
  
  expect_true(dataset::is.dataset_df(survey))
  
  bib <- attr(survey, "dataset_bibentry")
  
  expect_equal(bib$Title, "Untitled Survey")
  
  survey_subject <- attr(survey, "subject")
  expect_equal(survey_subject[[1]]$term, "Surveys")
  expect_equal(
    survey_subject[[1]]$valueURI,
    "http://id.loc.gov/authorities/subjects/sh85130875"
  )
})

test_that("is.survey_df() identifies survey_df objects", {
  
  survey <- survey_df(
    x = data.frame(x = 1),
    identifier = "example",
    filename = "file.csv"
  )
  
  expect_true(is.survey_df(survey))
  expect_false(is.survey_df(data.frame(x = 1)))
})


test_that("print.survey_df() works without error", {
  
  survey <- survey_df(
    x = data.frame(x = 1),
    title = "Example Survey",
    identifier = "example",
    filename = "file.csv"
  )
  
  expect_invisible(print(survey))
})

