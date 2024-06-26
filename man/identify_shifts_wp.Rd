% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_shifts_wp.R
\name{identify_shifts_wp}
\alias{identify_shifts_wp}
\title{Identify shifts based on binary activity}
\usage{
identify_shifts_wp(
  data,
  signals = c("email", "IM"),
  active_threshold = 1,
  start_hour = 9,
  end_hour = 17,
  percent = FALSE,
  n = 10,
  return = "plot"
)
}
\arguments{
\item{data}{A data frame containing data from the Hourly Collaboration query.}

\item{signals}{Character vector to specify which collaboration metrics to
use:
\itemize{
\item a combination of signals, such as \code{c("email", "IM")} (default)
\item \code{"email"} for emails only
\item \code{"IM"} for Teams messages only
\item \code{"unscheduled_calls"} for Unscheduled Calls only
\item \code{"meetings"} for Meetings only
}}

\item{active_threshold}{A numeric value specifying the minimum number of
signals to be greater than in order to qualify as \emph{active}. Defaults to 0.}

\item{start_hour}{A character vector specifying starting hours, e.g.
\code{"0900"}. Note that this currently only supports \strong{hourly} increments. If
the official hours specifying checking in and 9 AM and checking out at 5
PM, then \code{"0900"} should be supplied here.}

\item{end_hour}{A character vector specifying starting hours, e.g. \code{"1700"}.
Note that this currently only supports \strong{hourly} increments. If the
official hours specifying checking in and 9 AM and checking out at 5 PM,
then \code{"1700"} should be supplied here.}

\item{percent}{Logical value to determine whether to show labels as
percentage signs. Defaults to \code{FALSE}.}

\item{n}{Numeric value specifying number of shifts to show. Defaults to 10.
This parameter is only used when \code{return} is set to \code{"plot"},}

\item{return}{String specifying what to return. This must be one of the
following strings:
\itemize{
\item \code{"plot"}
\item \code{"table"}
\item \code{"data"}
}

See \code{Value} for more information.}
}
\value{
A different output is returned depending on the value passed to the \code{return}
argument:
\itemize{
\item \code{"plot"}: ggplot object. A bar plot for the weekly count of shifts.
\item \code{"table"}: data frame. A summary table for the count of shifts.
\item \verb{"data}: data frame. Input data appended with the following columns:
\itemize{
\item \code{Start}
\item \code{End}
\item \code{DaySpan}
\item \code{Shifts}
}
}
}
\description{
This function uses the Hourly Collaboration query and computes binary
activity to identify the 'behavioural' work shift. This is a distinct method
to \code{identify_shifts()}, which instead uses outlook calendar settings for
start and end time of work day to identify work shifts. The two methods can
be compared to gauge the accuracy of existing Outlook settings.
}
\examples{
# Return plot
em_data \%>\% identify_shifts_wp()

# Return plot - showing percentages
em_data \%>\% identify_shifts_wp(percent = TRUE)

# Return table
em_data \%>\% identify_shifts_wp(return = "table")

}
\seealso{
Other Data Validation: 
\code{\link{check_query}()},
\code{\link{extract_hr}()},
\code{\link{flag_ch_ratio}()},
\code{\link{flag_em_ratio}()},
\code{\link{flag_extreme}()},
\code{\link{flag_outlooktime}()},
\code{\link{hr_trend}()},
\code{\link{hrvar_count}()},
\code{\link{hrvar_count_all}()},
\code{\link{hrvar_trend}()},
\code{\link{identify_churn}()},
\code{\link{identify_holidayweeks}()},
\code{\link{identify_inactiveweeks}()},
\code{\link{identify_nkw}()},
\code{\link{identify_outlier}()},
\code{\link{identify_privacythreshold}()},
\code{\link{identify_query}()},
\code{\link{identify_shifts}()},
\code{\link{identify_tenure}()},
\code{\link{remove_outliers}()},
\code{\link{standardise_pq}()},
\code{\link{subject_validate}()},
\code{\link{subject_validate_report}()},
\code{\link{track_HR_change}()},
\code{\link{validation_report}()}

Other Working Patterns: 
\code{\link{flex_index}()},
\code{\link{identify_shifts}()},
\code{\link{plot_flex_index}()},
\code{\link{workpatterns_area}()},
\code{\link{workpatterns_classify}()},
\code{\link{workpatterns_classify_bw}()},
\code{\link{workpatterns_classify_pav}()},
\code{\link{workpatterns_hclust}()},
\code{\link{workpatterns_rank}()},
\code{\link{workpatterns_report}()}
}
\concept{Data Validation}
\concept{Working Patterns}
