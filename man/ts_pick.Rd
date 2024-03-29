% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_pick.R
\name{ts_pick}
\alias{ts_pick}
\title{Pick Series (Experimental)}
\usage{
ts_pick(x, ...)
}
\arguments{
\item{x}{ts-boxable time series, an object of class \code{ts}, \code{xts}, \code{zoo},
\code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl}, \code{tbl_ts}, \code{tbl_time}, \code{tis},
\code{irts} or \code{timeSeries}.}

\item{...}{character string(s), names of the series to be picked, or integer,
with positions. If arguments are named, the series will be renamed.}
}
\value{
a ts-boxable object of the same class as \code{x}, i.e., an object of
class \code{ts}, \code{xts}, \code{zoo}, \code{zooreg}, \code{data.frame}, \code{data.table}, \code{tbl},
\code{tbl_ts}, \code{tbl_time}, \code{tis}, \code{irts} or \code{timeSeries}.
}
\description{
Pick (and optionally rename) series from multiple time series.
}
\examples{
# Interactive use
\donttest{
ts_plot(ts_pick(
  EuStockMarkets,
  `My Dax` = "DAX",
  `My Smi` = "SMI"
))
ts_pick(EuStockMarkets, c(1, 2))
ts_pick(EuStockMarkets, `My Dax` = "DAX", `My Smi` = "SMI")

# Programming use
to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
ts_pick(EuStockMarkets, to.be.picked.and.renamed)
}

}
