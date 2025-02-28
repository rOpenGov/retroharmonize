% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_surveys.R
\name{subset_survey_memory}
\alias{subset_survey_memory}
\title{Subset surveys in memory}
\usage{
subset_survey_memory(
  this_survey,
  subset_vars,
  subset_name = "subset",
  export_path = NULL
)
}
\arguments{
\item{subset_vars}{The names of the variables that should be kept from all surveys in the list that contains the
wave of surveys. Defaults to \code{NULL} in which case it returns all variables without subsetting.}

\item{subset_name}{An identifier for the survey subset.}
}
\description{
Subset surveys in memory
}
\keyword{internal}
