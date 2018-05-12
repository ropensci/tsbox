#' Plot Time Series, Using ggplot2
#'
#' `ts_ggplot()` has the same syntax and produces a similar plot as [ts_plot()],
#' but uses the [ggplot2](http://ggplot2.org/) graphic system, and can be
#' customized. With [theme_tsbox()] and [scale_color_tsbox()], the output of
#' `ts_ggplot` has a similar look and feel.
#' 
#' Both [ts_plot()] and `ts_ggplot()` combine multiple ID dimensions into a single
#' dimension. To plot multiple dimensions in different shapes, facets, etc., use
#' standard ggplot (see examples).
#'
#' @param ... ts-boxable time series, objects of class `ts`, `xts`,
#'   `data.frame`, `data.table`, or `tibble`. For `scale_` functions, arguments
#'   passed to subfunctions.
#' @param title title (optional)
#' @param subtitle subtitle (optional)
#' @param ylab ylab (optional)
#' @param base_family base font family (can also be set via `options`)
#' @param base_size base font size
#' @seealso [ts_plot()], for a simpler and faster plotting function. 
#'   [ts_dygraphs()], for interactive time series plots.
#' @examples
#' \donttest{
#' # using the ggplot2 graphic system
#' p <- ts_ggplot(total = ldeaths, female = fdeaths, male = mdeaths)
#' p
#' 
#' # with themes for the look and feel of ts_plot()
#' p + theme_tsbox() + scale_color_tsbox()
#'
#' # also use themes with standard ggplot
#' suppressMessages(library(ggplot2))
#' df <- ts_df(ts_c(total = ldeaths, female = fdeaths, male = mdeaths))
#' ggplot(df, aes(x = time, y = value)) +
#'   facet_wrap("id") + 
#'   geom_line() +
#'   theme_tsbox() +
#'   scale_color_tsbox()
#' }
#'
#' \dontrun{
#' library(dataseries)
#' dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
#' ts_ggplot(ts_scale(ts_span(
#'     ts_c(
#'       `GDP Growth` = ts_pc(dta[, 'GDP.PBRTT.A.R']),
#'       `Consumer Sentiment Index` = dta[, 'CCI.CCIIR']
#'     ),
#'     start = "1995-01-01"
#'   ))) +
#'   ggplot2::ggtitle("GDP and Consumer Sentiment", subtitle = "normalized values") +
#'   theme_tsbox() +
#'   scale_color_tsbox()
#' }
#' @export
ts_ggplot <- function(..., title, subtitle, ylab = "") {
  stopifnot(requireNamespace("ggplot2"))
  x <- ts_dts(ts_c(...))

  # only a single id col
  x <- combine_id_cols(x)

  cname <- dts_cname(x)

  df <- ts_df(x)
  df <- df[!is.na(df[, cname$value]), ]

  n <- NCOL(df)
  stopifnot(n > 1)
  if (n == 2) {
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = cname$time, y = cname$value))
  } else if (n > 2) {

    # numeric variable 'levels'
    if (class(df[[cname$id]]) %in% c("integer", "numeric")) {
      df[[cname$id]] <- as.character(df[[cname$id]])
    }

    if (length(unique(df[[cname$id]])) > 29) {
      stop(length(unique(df[[cname$id]])), " time series supplied. Maximum is 29.", call. = FALSE)
    }
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = cname$time, y = cname$value, color = cname$id))
  }
  p <- p + ggplot2::geom_line()

  # labels and title
  p <- p + ggplot2::ylab(ylab)
  if (!missing("title")) {
    if (missing("subtitle")) subtitle <- NULL
    p <- p +  ggplot2::ggtitle(label = title, subtitle = subtitle)
  }

  p
}


#' @export
#' @name ts_ggplot
theme_tsbox <- function(base_family = getOption("ts_font", ""), base_size = 12) {
  # 'Source Sans Pro'  # does not work on mac
  # 'Slabo 13px'

  half_line <- base_size / 2
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(

      # line = ggplot2::element_line(color = "grey30", size = 0.4),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = ggplot2::rel(0.9), color = "grey10",
        margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)
      ),
      plot.title = ggplot2::element_text(color = "grey10", face = "bold", margin = ggplot2::margin(t = half_line * 2, b = half_line * 0.7), hjust = 0, size = ggplot2::rel(1.2)),
      plot.subtitle = ggplot2::element_text(color = "grey10", margin = ggplot2::margin(t = 0, b = half_line * 1.2), size = ggplot2::rel(0.9), hjust = 0),
      plot.caption = ggplot2::element_text(color = "grey50", margin = ggplot2::margin(t = 0, b = half_line * 1.2), size = ggplot2::rel(0.8)),

      # panel.grid = ggplot2::element_line(colour = NULL, linetype = 3),
      # panel.grid.major = ggplot2::element_line(colour = "grey10"),
      panel.grid = ggplot2::element_line(size = 0.2),
      # panel.grid.major.x = ggplot2::element_blank(),
      # panel.grid.minor = ggplot2::element_blank(),

      axis.text = ggplot2::element_text(color = "grey10", size = ggplot2::rel(0.7)),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = "grey10", size = ggplot2::rel(0.9)),
      legend.position = "bottom",
      legend.direction = "horizontal"

      # axis.ticks.x = ggplot2::element_line(color = "grey10"),
      # axis.ticks.y = ggplot2::element_blank(),
      # panel.border = ggplot2::element_blank(),
      # axis.line.x =  ggplot2::element_line()
    )
}


#' @export
#' @name ts_ggplot
colors_tsbox <- function() {
  c(
    "#4D4D4D",
    "#5DA5DA",
    "#FAA43A",
    "#60BD68",
    "#F15854",
    "#B276B2",
    "#DECF3F",
    "#F17CB0",
    "#B2912F",
    "#4afff0", "#34bdcc", "#4f61a1", "#461e78", "#440a4f", "#c3fbc4",
    "#85f9d6", "#79c7ad", "#a6cc7a", "#dfff7b",
    "#8d7b88", "#4e414f", "#baadb5", "#2d2538", "#837a80", "#fff68f",
    "#800080", "#f8b1cc", "#c29bff", "#8d0808"
  )
}

#' @export
#' @name ts_ggplot
scale_color_tsbox <- function(...) {
  stopifnot(requireNamespace("ggplot2"))
  ggplot2::discrete_scale("colour", "ds", scales::manual_pal(colors_tsbox()), ...)
}

#' @export
#' @name ts_ggplot
scale_fill_tsbox <- function(...) {
  stopifnot(requireNamespace("ggplot2"))
  ggplot2::discrete_scale("fill", "ds", scales::manual_pal(colors_tsbox()), ...)
}




