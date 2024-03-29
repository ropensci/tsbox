% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_from_all.R
\name{relevant_class}
\alias{relevant_class}
\title{Extract Relevant Class}
\usage{
relevant_class(x)
}
\arguments{
\item{x}{ts-boxable time series, an object of class \code{ts}, \code{xts}, \code{zoo},
\code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl}, \code{tbl_ts}, \code{tbl_time}, \code{tis},
\code{irts} or \code{timeSeries}.}
}
\value{
character, the relevant class of ts-boxable object
}
\description{
Mainly used internally.
}
\examples{
relevant_class(AirPassengers)
x <- ts_df(AirPassengers)
relevant_class(x)
}
