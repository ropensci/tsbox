% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_summary.R
\name{ts_summary}
\alias{ts_summary}
\title{Time Series Properties}
\usage{
ts_summary(x, spark = FALSE)
}
\arguments{
\item{x}{ts-boxable time series, an object of class \code{ts}, \code{xts}, \code{zoo},
\code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl}, \code{tbl_ts}, \code{tbl_time}, \code{tis},
\code{irts} or \code{timeSeries}.}

\item{spark}{logical should an additional column with a spark-line added to
the data frame (experimental, ASCII only on Windows.)}
}
\value{
\code{ts_summary} returns a \code{data.frame}. Individual column can be
accessed through the \code{$} notation (see examples).
}
\description{
Extract time series properties, such as the number of observations
(\code{obs}), the time differences between observations (\code{obs}), the number
of observations per year (\code{freq}), and the start time stamp (\code{start})
and the end time stamp (\code{end}) of the series.
}
\examples{
ts_summary(ts_c(mdeaths, austres))
ts_summary(ts_c(mdeaths, austres), spark = TRUE)
# Extracting specific properties
ts_summary(AirPassengers)$start
ts_summary(AirPassengers)$freq
ts_summary(AirPassengers)$obs
}
