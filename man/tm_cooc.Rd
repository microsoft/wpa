% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_cooc.R
\name{tm_cooc}
\alias{tm_cooc}
\title{Analyse word co-occurrence in subject lines and return a network plot}
\usage{
tm_cooc(data, stopwords = NULL, seed = 100, return = "plot", lmult = 0.05)
}
\arguments{
\item{data}{A Meeting Query dataset in the form of a data frame.}

\item{stopwords}{A single-column data frame labelled 'word' containing custom stopwords to remove.}

\item{seed}{A numeric vector to set seed for random generation.}

\item{return}{Character vector specifying what to return, defaults to "plot".
Valid inputs are "plot" and "table".}

\item{lmult}{A multiplier to adjust the line width in the output plot. Defaults to 0.05.}
}
\description{
This function generates a word co-occurence network plot,
with options to return a table.
This is a sub-function that feeds into \code{meeting_tm_report()}.
}
\details{
This function uses \code{tm_clean()} as the underlying data wrangling function.
There is an option to remove stopwords by passing a data frame into the \code{stopwords}
argument.
}
\examples{
\dontrun{
tm_cooc(mt_data,lmult = 0.01)
}
}
\seealso{
Other Text-mining: 
\code{\link{meeting_tm_report}()},
\code{\link{tm_clean}()},
\code{\link{tm_freq}()},
\code{\link{tm_wordcloud}()}
}
\concept{Text-mining}