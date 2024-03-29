.ts_lastplot_env <- new.env(parent = emptyenv())
#' Plot Time Series
#'
#' `ts_plot()` is a fast and simple plotting function for ts-boxable time
#' series, with limited customizability. For more theme options, use
#' [ts_ggplot()].
#'
#' Both `ts_plot()` and [ts_ggplot()] combine multiple ID dimensions into a
#' single dimension. To plot multiple dimensions in different shapes, facets,
#' etc., use standard ggplot.
#'
#' Limited customizability of `ts_plot` is available via options. See examples.
#'
#' @param ... ts-boxable time series, an object of class `ts`, `xts`, `zoo`,
#'   `zooreg`, `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`,
#'   `irts` or `timeSeries`.
#' @param title title (optional)
#' @param subtitle subtitle (optional)
#' @param ylab ylab (optional)
#' @param family font family (optional, can also be set via `options`)
#' @seealso [ts_ggplot()], for a plotting function based on ggplot2.
#'   [ts_dygraphs()], for interactive time series plots. [ts_save()] to
#'   save a plot to the file system.
#' @examples
#' \donttest{
#' ts_plot(
#'   AirPassengers,
#'   title = "Airline passengers",
#'   subtitle = "The classic Box & Jenkins airline data"
#' )
#'
#' # naming arguments
#' ts_plot(total = ldeaths, female = fdeaths, male = mdeaths)
#'
#' # using different ts-boxable objects
#' ts_plot(ts_scale(ts_c(
#'   ts_xts(airmiles),
#'   ts_tbl(co2),
#'   JohnsonJohnson,
#'   ts_df(discoveries)
#' )))
#'
#' # customize ts_plot
#' op <- options(
#'   tsbox.lwd = 3,
#'   tsbox.col = c("gray51", "gray11"),
#'   tsbox.lty = "dashed"
#' )
#' ts_plot(
#'   "Female" = fdeaths,
#'   "Male" = mdeaths
#' )
#' options(op) # restore defaults
#' }
#' @export
#' @importFrom graphics abline axis axTicks legend lines mtext par plot
#' @importFrom grDevices dev.off pdf bmp jpeg png tiff
#'
#' @srrstats {TS5.0} *Implement default `plot` methods for any implemented class system.*
#'   `ts_plot()` works for any of the supported classes. Since there are defaul `plot` methods for many of the supported classes, it must be a nwe function. `ts_ggplot()` is an alternative.
#' @srrstats{TS5.2} *Default to placing the "time" (or equivalent) variable on the horizontal axis.*
#'   Done.
ts_plot <- function(..., title, subtitle, ylab = "",
                    family = getOption("ts_font", "sans")) {
  value <- NULL
  id <- NULL

  x <- ts_dts(ts_c(...))

  if (nrow(ts_na_omit(x)) == 0L) stop0("no data values to plot")


  # only a single id col
  x <- combine_id_cols(x)


  if (missing("title")) {
    has.title <- FALSE
  } else {
    has.title <- TRUE
  }

  if (missing("subtitle")) {
    has.subtitle <- FALSE
  } else {
    has.subtitle <- TRUE
  }


  op <- par(no.readonly = TRUE) # restore par on exit
  # on.exit(par(op))

  cex <- 0.9
  title.cex <- 1.2
  text.col <- "grey10"

  axis.text.col <- text.col
  # axis.text.col <- "grey50"

  col.lab <- axis.text.col

  # c(bottom, left, top, right)
  if (number_of_series(x) > 1) {
    has.legend <- TRUE
  } else {
    has.legend <- FALSE
  }

  if (has.legend) {
    mar.b <- 4.5
  } else {
    mar.b <- 2
  }

  if (has.title && has.subtitle) {
    mar.t <- 4
  } else if (has.title || has.subtitle) {
    mar.t <- 3
  } else {
    mar.t <- 1
  }

  if (ylab == "") {
    mar.l <- 3
  } else {
    mar.l <- 4
  }


  # First layer with legend and title
  par(
    fig = c(0, 1, 0, 1), oma = c(0.5, 1, 2, 1), mar = c(0, 0, 0, 0),
    col.lab = col.lab, cex.lab = 0.8, family = family
  )

  # empty plot
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

  if (has.title) {
    mtext(
      title,
      cex = title.cex, side = 3, line = 0, adj = 0, font = 2, col = text.col
    )
  }
  if (has.subtitle) {
    shift <- if (has.title) -1.2 else 0
    mtext(
      subtitle,
      cex = cex, side = 3, line = shift, adj = 0, col = text.col
    )
  }


  # bring into canonical form
  cid <- dts_cname(x)$id
  ctime <- dts_cname(x)$time
  cvalue <- dts_cname(x)$value

  if (length(cid) == 0L) {
    x$id <- "dummy"
    cid <- "id"
    setcolorder(x, c("id", ctime, cvalue))
  }
  setnames(
    x, c(cid, ctime, cvalue),
    c("id", "time", "value")
  )

  # time vector
  tind <- as.POSIXct(x[, time])
  tnum <- as.numeric(tind)
  xlim <- range(tnum)
  # value vector
  values <- x[, value]
  values[!is.finite(values)] <- NA # Inf is not accepted for ylim
  ylim <- range(values, na.rm = TRUE)

  xticks <- pretty(tind)
  xlabels <- attr(xticks, "labels")

  # Lines
  ids <- as.character(unique(x[, id]))
  if ((length(ids)) > 20L) {
    message("too many series. Only showing the first 20.")
    ids <- ids[1:20]
  }

  # graphical parameters, via options
  col <- getOption("tsbox.col", colors_tsbox())
  lty <- getOption("tsbox.lty", "solid")
  lwd <- getOption("tsbox.lwd", 1.5)

  recycle_par <- function(x) {
    x0 <- x[1:(min(length(x), length(ids)))]
    cbind(ids, x0)[, 2]
  }

  col <- recycle_par(col)
  lty <- recycle_par(lty)
  lwd <- recycle_par(lwd)

  if (has.legend) {
    legend(
      "bottomleft",
      legend = ids, horiz = TRUE,
      bty = "n", lty = lty, lwd = lwd, col = col, cex = 0.9 * cex, adj = 0,
      text.col = text.col
    )
  }

  # Second layer with graph
  # (that way, we can further process the graph later on)
  par(mar = c(mar.b, mar.l, mar.t, 1.4), new = TRUE)

  # Main Plot
  plot(
    x = tind, type = "n", lty = lty[1], pch = 19, col = 1,
    cex = 1.5, lwd = lwd[1], las = 1, bty = "n", xaxt = "n",
    xlim = xlim, ylim = ylim, xlab = "", ylab = ylab,
    yaxt = "n"
  )

  axis(
    2,
    at = axTicks(2), labels = sprintf("%s", axTicks(2)),
    las = 1, cex.axis = 0.8, col = NA, line = -0.5, col.axis = axis.text.col
  )

  axis(
    side = 1, at = (xticks),
    labels = xlabels,
    las = 1, cex.axis = 0.8, col = NA, line = 0.5, tick = TRUE, padj = -2,
    col.axis = axis.text.col
  )

  # Gridlines
  abline(h = axTicks(2), v = xticks, col = "grey80", lty = "dotted", lwd = 0.5)

  for (i in seq(ids)) {
    .idi <- ids[i]
    cd <- x[id == .idi] # this will be named id
    cd <- cd[!is.na(value)]
    x.i <- as.numeric(as.POSIXct(cd[, time]))
    lines(
      y = cd[, value],
      x = x.i, col = col[i], lty = lty[i], lwd = lwd[i]
    )
  }

  cl <- match.call()
  assign("ts_lastplot_call", cl, envir = .ts_lastplot_env)
}


#' Get Last Plot Call
#'
#' Last plot call is written to special enviroment by ts_plot()
#'
#' @noRd
ts_lastplot_call <- function() {
  get("ts_lastplot_call", envir = .ts_lastplot_env)
}


#' Save Previous Plot
#'
#' @param filename filename
#' @param width width
#' @param height height
#' @param device device
#' @param open logical, should the saved plot be opened?
#' @return invisible `TRUE`, if successful
#' @export
#' @examples
#' \donttest{
#' ts_plot(AirPassengers)
#' tf <- tempfile(fileext = ".pdf")
#' ts_save(tf)
#' unlink(tf)
#' }
#'
#' @srrstats {G4.0} *Statistical Software which enables outputs to be written
#' to local files should parse parameters specifying file names to ensure
#' appropriate file suffices are automatically generated where not provided.*
ts_save <- function(filename = tempfile(fileext = ".pdf"), width = 10,
                    height = 5, device = NULL, open = TRUE) {

  # if no file suffix is provided use .pdf
  if (!grepl("\\.[a-z]+$", filename) && is.null(device)) {
    filename <- paste0(filename, ".pdf")
  }

  if (is.null(device)) {
    device <- gsub(".*\\.([a-z]+)$", "\\1", tolower(filename))
  } else {
    filename <- gsub("\\.[a-z]+$", paste0(".", device), tolower(filename))
  }

  filename <- normalizePath(filename, mustWork = FALSE)

  cl <- ts_lastplot_call()
  if (is.null(cl) || !inherits(cl, "call")) {
    stop0("ts_plot() must be called first.")
  }

  if (device == "pdf") {
    pdf(file = filename, width = width, height = height)
  } else if (device == "png") {
    png(
      filename = filename, width = width, height = height, units = "in",
      res = 150
    )
  } else if (device == "bmp") {
    bmp(
      filename = filename, width = width, height = height, units = "in",
      res = 150
    )
  } else if (device == "jpeg") {
    jpeg(
      filename = filename, width = width, height = height, units = "in",
      res = 150
    )
  } else if (device == "tiff") {
    tiff(
      filename = filename, width = width, height = height, units = "in",
      res = 150
    )
  } else {
    stop0("device not supported: ", device)
  }

  eval(cl, envir = parent.frame())
  dev.off()

  if (open) browseURL(filename)

  invisible(TRUE)
}
