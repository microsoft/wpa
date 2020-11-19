% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_nkw.R
\name{identify_nkw}
\alias{identify_nkw}
\title{Identify Non-Knowledge workers in a Person Query using Collaboration Hours}
\usage{
identify_nkw(data, collab_threshold = 5, return = "data_summary")
}
\arguments{
\item{data}{A Standard Query dataset in the form of a data frame.}

\item{collab_threshold}{The collaboration hours threshold that should be exceeded as an average for the entire analysis period for the employee to be categorized as a knowledge worker ("kw").
Enter a positive number. Default is set to 5 collaboration hours.}

\item{return}{String to specify what to return}
}
\value{
Returns an error message by default, where 'text' is passed in \code{return}.
When 'data_with_flag' is passed, a copy of your original dataframe with an additional column containing the kw/nkw flag will be returned.
When 'data_clean' is passed, a full dataframe excluding "nkw" employees will be returned.
when 'data_summmary' is passed, a summary table by organization listing the number and \% of non-knowledge workers will be returned.
}
\description{
This function scans a standard query output to identify employees with consistently low collaboration signals.
Returns the \% of non-knowledge workers identified by Organization, and optionally an edited dataframe with non-knowledge workers removed, or the full dataframe with the kw/nkw flag added.
}
\seealso{
Other Data Validation: 
\code{\link{check_query}()},
\code{\link{flag_ch_ratio}()},
\code{\link{flag_em_ratio}()},
\code{\link{flag_extreme}()},
\code{\link{flag_outlooktime}()},
\code{\link{hrvar_count_all}()},
\code{\link{hrvar_count}()},
\code{\link{identify_holidayweeks}()},
\code{\link{identify_inactiveweeks}()},
\code{\link{identify_outlier}()},
\code{\link{identify_privacythreshold}()},
\code{\link{identify_query}()},
\code{\link{identify_tenure}()},
\code{\link{remove_outliers}()},
\code{\link{subject_validate_report}()},
\code{\link{subject_validate}()},
\code{\link{track_HR_change}()}
}
\concept{Data Validation}