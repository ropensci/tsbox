% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_lag.R
\name{ts_lag}
\alias{ts_lag}
\title{Lag or Lead of Time Series}
\usage{
ts_lag(x, by = 1)
}
\arguments{
\item{x}{ts-boxable time series, an object of class \code{ts}, \code{xts}, \code{zoo},
\code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl}, \code{tbl_ts}, \code{tbl_time}, \code{tis},
\code{irts} or \code{timeSeries}.}

\item{by}{integer or character, either the number of shifting periods
(integer), or an absolute amount of time (character). See details.}
}
\value{
a ts-boxable object of the same class as \code{x}, i.e., an object of
class \code{ts}, \code{xts}, \code{zoo}, \code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl},
\code{tbl_ts}, \code{tbl_time}, \code{tis}, \code{irts} or \code{timeSeries}.
}
\description{
Shift time stamps in ts-boxable time series, either by a number of periods or
by a fixed amount of time.
}
\details{
The lag order, \code{by}, is defined the opposite way as in R base. Thus, -1 is a
lead and +1 a lag.

If \code{by} is integer, the time stamp is shifted by the number of periods. This
requires the series to be regular.

If \code{by} is character, the time stamp is shifted by a specific amount of time.
This can be one of one of \code{"sec"}, \code{"min"}, \code{"hour"}, \code{"day"}, \code{"week"},
\code{"month"}, \verb{"quarter" or }"year", optionally preceded by a (positive or
negative) integer and a space, or followed by plural "s". This is passed to
\code{\link[base:seq.Date]{base::seq.Date()}}. This does not require the series to be regular.
}
\examples{
\donttest{
ts_plot(AirPassengers, ts_lag(AirPassengers), title = "The need for glasses")
}
ts_lag(fdeaths, "1 month")
ts_lag(fdeaths, "1 year")
x <- ts_df(fdeaths)
ts_lag(x, "2 day")
ts_lag(x, "2 min")
ts_lag(x, "-1 day")
}
