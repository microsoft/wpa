% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wellbeing_report.R
\name{wellbeing_report}
\alias{wellbeing_report}
\title{Generate a Wellbeing Report in HTML}
\usage{
wellbeing_report(
  wbq,
  hcq,
  hrvar = "Organization",
  mingroup = 5,
  start_hour = "0900",
  path = "wellbeing_report"
)
}
\arguments{
\item{wbq}{Data frame. A custom Wellbeing Query dataset based on the Person
Query.}

\item{hcq}{Data frame. An Hourly Collaboration Query dataset.}

\item{hrvar}{String specifying HR attribute to cut by archetypes. Defaults to
\code{Organization}.}

\item{mingroup}{Numeric value setting the privacy threshold / minimum group
size. Defaults to 5.}

\item{start_hour}{A character vector specifying starting hours, e.g. "0900".
Note that this currently only supports hourly increments.}

\item{path}{Pass the file path and the desired file name, \emph{excluding the file
extension}. Defaults to \code{"wellbeing_report"}.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function takes a custom Wellbeing Query and an Hourly Collaboration
query, generating a HTML report on wellbeing.
}
