% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capacity_report.R
\name{capacity_report}
\alias{capacity_report}
\title{Generate a Capacity report in HTML}
\usage{
capacity_report(
  data,
  hrvar = "Organization",
  mingroup = 5,
  path = "capacity report",
  timestamp = TRUE
)
}
\arguments{
\item{data}{A Standard Person Query dataset in the form of a data frame.}

\item{hrvar}{HR Variable by which to split metrics, defaults to "Organization"
but accepts any character vector, e.g. "LevelDesignation"}

\item{mingroup}{Numeric value setting the privacy threshold / minimum group size. Defaults to 5.}

\item{path}{Pass the file path and the desired file name, \emph{excluding the file extension}.
For example, "capacity report".}

\item{timestamp}{Logical vector specifying whether to include a timestamp in the file name.
Defaults to TRUE.}
}
\description{
The function generates an interactive HTML report using the
Standard Person Query data as an input. The report contains a series
of summary analysis and visualisations relating to key \strong{capacity}
metrics in Workplace Analytics,including length of week and
time in after-hours collaboration.
}
\seealso{
Other Reports: 
\code{\link{IV_report}()},
\code{\link{coaching_report}()},
\code{\link{collaboration_report}()},
\code{\link{connectivity_report}()},
\code{\link{generate_report}()},
\code{\link{validation_report}()}
}
\concept{Reports}