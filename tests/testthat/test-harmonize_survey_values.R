example_survey_paths <- function() {
  examples_dir <- system.file("examples", package = "retroharmonize")
  dir(examples_dir, pattern = "\\.rds$", full.names = TRUE)
}

example_supported_vars <- function(s) {
  s[, vapply(
    s,
    function(x) inherits(x, c(
      "retroharmonize_labelled_spss_survey",
      "numeric",
      "character",
      "Date"
    )),
    logical(1)
  )]
}

test_that("harmonize_survey_values errors on unsupported variable types", {
  
  s <- read_rds(
    system.file("examples", "ZA7576.rds", package = "retroharmonize")
  )
  
  # Inject unsupported type: logical
  s$logical_flag <- s$rowid == s$rowid
  
  expect_error(
    harmonize_survey_values(
      survey_list = list(s),
      .f = identity,
      status_message = FALSE
    ),
    "Only labelled_spss_survey, numeric, character and Date types are allowed"
  )
})


test_that(
  "harmonize_survey_values works on example surveys after
  filtering supported types",
  {
    
    examples_dir <- system.file("examples", package = "retroharmonize")
    survey_files <- dir(examples_dir, pattern = "\\.rds$", full.names = TRUE)
    
    surveys <- read_surveys(
      survey_files,
      export_path = NULL
    )
    
    surveys <- lapply(
      surveys,
      function(s) {
        s[, vapply(
          s,
          function(x) inherits(x, c(
            "retroharmonize_labelled_spss_survey",
            "numeric",
            "character",
            "Date"
          )),
          logical(1)
        )]
      }
    )
    
    expect_no_error(
      harmonize_survey_values(
        survey_list = surveys,
        .f = function(x) x,
        status_message = FALSE
      )
    )
  }
)
