% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_from_all.R
\name{ts_boxable}
\alias{ts_boxable}
\alias{check_ts_boxable}
\title{Test if an Object is ts-Boxable}
\usage{
ts_boxable(x)

check_ts_boxable(x)
}
\arguments{
\item{x}{ts-boxable time series, an object of class \code{ts}, \code{xts}, \code{zoo},
\code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl}, \code{tbl_ts}, \code{tbl_time}, \code{tis},
\code{irts} or \code{timeSeries}.}
}
\value{
logical, either \code{TRUE} or \code{FALSE}. \code{check_ts_boxable()} fails if not
\code{TRUE}
}
\description{
Mainly used internally.
}
\examples{
ts_boxable(AirPassengers)
ts_boxable(lm)
}
