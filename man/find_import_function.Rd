% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_surveys.R
\name{find_import_function}
\alias{find_import_function}
\title{Find import function by file extension}
\usage{
find_import_function(file_path)
}
\value{
The name of the function that should read \code{file_path} based on the file 
extension.
}
\description{
This is an internal utility to select the appropriate importing function.
}
\keyword{internal}
