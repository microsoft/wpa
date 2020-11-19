% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collaboration_area.R
\name{collaboration_area}
\alias{collaboration_area}
\alias{collab_area}
\title{Collaboration - Stacked Area Plot}
\usage{
collaboration_area(data, hrvar = "Organization", mingroup = 5, return = "plot")

collab_area(data, hrvar = "Organization", mingroup = 5, return = "plot")
}
\arguments{
\item{data}{A Standard Query dataset in the form of a data frame.
A Collaboration assessment dataset may also be provided, in which
Unscheduled call hours would be included in the output.}

\item{hrvar}{HR Variable by which to split metrics, defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"}

\item{mingroup}{Numeric value setting the privacy threshold / minimum group size. Defaults to 5.}

\item{return}{Character vector specifying what to return, defaults to "plot".
Valid inputs are "plot" and "table".}
}
\value{
Returns a ggplot object by default, where 'plot' is passed in \code{return}.
When 'table' is passed, a summary table is returned as a data frame.
}
\description{
Provides an overview analysis of 'Weekly Digital Collaboration'.
Returns an stacked area plot of Email and Meeting Hours by default.
Additional options available to return a summary table.
}
\details{
Uses the metrics \code{Meeting_hours}, \code{Email_hours}, \code{Unscheduled_Call_hours},
and \code{Instant_Message_hours}.
}
\seealso{
Other Collaboration: 
\code{\link{collaboration_dist}()},
\code{\link{collaboration_fizz}()},
\code{\link{collaboration_line}()},
\code{\link{collaboration_rank}()},
\code{\link{collaboration_sum}()},
\code{\link{collaboration_trend}()}
}
\concept{Collaboration}