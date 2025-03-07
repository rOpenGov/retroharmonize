% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonize_na_values.R
\name{harmonize_na_values}
\alias{harmonize_na_values}
\title{Harmonize na_values in haven_labelled_spss}
\usage{
harmonize_na_values(df)
}
\arguments{
\item{df}{A data frame that contains haven_labelled_spss vectors.}
}
\value{
A tibble where the na_values are consistent
}
\description{
Harmonize na_values in haven_labelled_spss
}
\examples{
\donttest{
examples_dir <- system.file(
    "examples", package = "retroharmonize"
    )

test_read <- read_rds ( 
     file.path(examples_dir, "ZA7576.rds"),
     id = "ZA7576", 
     doi = "test_doi")

harmonize_na_values(test_read)
}
}
\seealso{
Other harmonization functions: 
\code{\link{collect_val_labels}()},
\code{\link{crosswalk_surveys}()},
\code{\link{crosswalk_table_create}()},
\code{\link{harmonize_survey_values}()},
\code{\link{harmonize_values}()},
\code{\link{harmonize_var_names}()},
\code{\link{label_normalize}()}
}
\concept{harmonization functions}
