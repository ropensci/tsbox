% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_ggplot.R
\name{ts_ggplot}
\alias{ts_ggplot}
\alias{theme_tsbox}
\alias{colors_tsbox}
\alias{scale_color_tsbox}
\alias{scale_fill_tsbox}
\title{Plot Time Series, Using ggplot2}
\usage{
ts_ggplot(..., title, subtitle, ylab = "")

theme_tsbox(base_family = getOption("ts_font", ""), base_size = 12)

colors_tsbox()

scale_color_tsbox(...)

scale_fill_tsbox(...)
}
\arguments{
\item{...}{ts-boxable time series, objects of class \code{ts}, \code{xts},
\code{data.frame}, \code{data.table}, or \code{tibble}. For \code{scale_} functions, arguments
passed to subfunctions.}

\item{title}{title (optional)}

\item{subtitle}{subtitle (optional)}

\item{ylab}{ylab (optional)}

\item{base_family}{base font family (can also be set via \code{options})}

\item{base_size}{base font size}
}
\description{
\code{ts_ggplot()} has the same syntax and produces a similar plot as \code{\link[=ts_plot]{ts_plot()}},
but uses the \href{https://ggplot2.tidyverse.org/}{ggplot2} graphic system, and
can be customized. With \code{\link[=theme_tsbox]{theme_tsbox()}} and \code{\link[=scale_color_tsbox]{scale_color_tsbox()}}, the output
of \code{ts_ggplot} has a similar look and feel.
}
\details{
Both \code{\link[=ts_plot]{ts_plot()}} and \code{ts_ggplot()} combine multiple ID dimensions into a
single dimension. To plot multiple dimensions in different shapes, facets,
etc., use standard ggplot (see examples).
}
\examples{
\donttest{
# using the ggplot2 graphic system
p <- ts_ggplot(total = ldeaths, female = fdeaths, male = mdeaths)
p

# with themes for the look and feel of ts_plot()
p + theme_tsbox() + scale_color_tsbox()

# also use themes with standard ggplot
suppressMessages(library(ggplot2))
df <- ts_df(ts_c(total = ldeaths, female = fdeaths, male = mdeaths))
ggplot(df, aes(x = time, y = value)) +
  facet_wrap("id") +
  geom_line() +
  theme_tsbox() +
  scale_color_tsbox()
}

\dontrun{
library(dataseries)
dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
ts_ggplot(ts_scale(ts_span(
  ts_c(
    `GDP Growth` = ts_pc(dta[, "GDP.PBRTT.A.R"]),
    `Consumer Sentiment Index` = dta[, "CCI.CCIIR"]
  ),
  start = "1995-01-01"
))) +
  ggplot2::ggtitle("GDP and Consumer Sentiment", subtitle = "normalized") +
  theme_tsbox() +
  scale_color_tsbox()
}
}
\seealso{
\code{\link[=ts_plot]{ts_plot()}}, for a simpler and faster plotting function.
\code{\link[=ts_dygraphs]{ts_dygraphs()}}, for interactive time series plots.
}
