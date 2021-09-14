% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_first_of_period.R
\name{ts_first_of_period}
\alias{ts_first_of_period}
\title{Use First Date of a Period}
\usage{
ts_first_of_period(x)
}
\arguments{
\item{x}{ts-boxable time series, an object of class \code{ts}, \code{xts}, \code{zoo},
\code{data.frame}, \code{data.table}, \code{tbl}, \code{tbl_ts}, \code{tbl_time}, \code{tis}, \code{irts} or
\code{timeSeries}.}
}
\value{
A ts-boxable object of the same class as the input.
}
\description{
Replace date or time values by the first of the period. tsbox usually relies
on timestamps being the first value of a period.
}
\examples{
x <- ts_c(
  a = ts_lag(ts_df(mdeaths), "14 days"),
  b = ts_lag(ts_df(mdeaths), "-2 days")
)
ts_first_of_period(x)
ts_first_of_period(ts_lag(ts_df(austres), "14 days"))
x <- ts_lag(data.frame(
  time = seq(as.POSIXct("1970-01-01"), length.out = 10, by = "10 sec"),
  value = rnorm(10)
), "3 sec")
ts_first_of_period(x)
}