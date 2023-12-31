% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lib-summary.R
\name{lib_summary}
\alias{lib_summary}
\title{R library summary}
\usage{
lib_summary(sizes = FALSE)
}
\arguments{
\item{sizes}{logical indicating whether or not to calculate
library sizes. Default is \code{FALSE}.}
}
\value{
A 'data.frame' containing the count of packages in each
of the users libraries
}
\description{
Provide the brief summary of package
libraries on your machine
}
\examples{
lib_summary()
}
