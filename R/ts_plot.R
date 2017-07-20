.tsbox <- new.env(parent = emptyenv())

#' Plot Time Series
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param title title (optional)
#' @param subtitle subtitle (optional)
#' @param ylab ylab (optional)
#' @param familiy font familiy (optional)
#' @examples
#' 
#' library(tsbox)
#' ts_plot(AirPassengers, title = "Airline passengers", 
#'        subtitle = "The classic Box & Jenkins airline data")
#' ts_plot(total = ldeaths, female = fdeaths, male = mdeaths)
#' 
#' ts_plot(ts_c(sunspot.month, sunspot.year, lynx))
#' ts_plot(ts_scale(ts_c(airmiles, co2, JohnsonJohnson, discoveries)))
#' ts_plot(EuStockMarkets)
#' ts_plot(sunspot.month, sunspot.year, lynx)
#' ts_plot(ts_scale(ts_c(Nile, nottem, USAccDeaths)))
#' \dontrun{
#' ts_ggplot(AirPassengers, title = "Airline passengers", 
#'        subtitle = "The classic Box & Jenkins airline data")
#' ts_ggplot(total = ldeaths, female = fdeaths, male = mdeaths)
#' 
#' ts_ggplot(ts_c(sunspot.month, sunspot.year, lynx))
#' ts_ggplot(ts_scale(ts_c(airmiles, co2, JohnsonJohnson, discoveries)))
#' ts_ggplot(EuStockMarkets)
#' ts_ggplot(sunspot.month, sunspot.year, lynx)
#' ts_ggplot(ts_scale(ts_c(Nile, nottem, USAccDeaths)))
#' 
#' library(Quandl)
#' ts_ggplot(Quandl("FRED/GDPMC1", "xts"), title = "US GDP")
#' 
#' library(dataseries)
#' dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
#' ts_ggplot(ts_scale(ts_window(ts_c(`GDP Growth` = ts_pc(dta[, 'GDP.PBRTT.A.R']), 
#'                             `Consumer Sentiment Index` = dta[, 'CCI.CCIIR']), 
#'                      start = "1995-01-01")),
#'        title = "GDP and Consumer Sentiment",
#'        subtitle = "normalized values")
#' 
#' }
#' @export
#' @importFrom graphics abline axis axTicks legend lines mtext par plot
#' @importFrom grDevices dev.off pdf bmp jpeg png tiff
ts_plot <- function(..., title, subtitle, ylab = "", family = "sans"){

  x <- ts_dts(ts_c(...))

  if (missing("title")){
    has.title <- FALSE
  } else {
    has.title <- TRUE
  }

  if (missing("subtitle")){
    has.subtitle <- FALSE
  } else {
    has.subtitle <- TRUE
  }


  op <- par(no.readonly = TRUE) # restore par on exit
  # on.exit(par(op))

  lwd <- 1.5
  cex <- 0.9
  title.cex <- 1.2
  text.col <- "grey10"

  axis.text.col <- text.col
  # axis.text.col <- "grey50"

  col.lab <- axis.text.col

  # c(bottom, left, top, right)
  if (ts_nvar(x) > 1){
    has.legend <- TRUE
  } else{
    has.legend <- FALSE
  }

  if (has.legend) {
    mar.b <- 4.5
  } else {
    mar.b <- 2
  }

  if (has.title && has.subtitle){
    mar.t <- 4
  } else if (has.title || has.subtitle){
    mar.t <- 3
  } else {
    mar.t <- 1
  }

  if (ylab == ""){
    mar.l <- 3
  } else {
    mar.l <- 4
  }


  # First layer with legend and title
  par(fig=c(0, 1, 0, 1), oma=c(0.5, 1, 2, 1), mar=c(0, 0, 0, 0), col.lab = col.lab, cex.lab = 0.8,
      family = family)

  # empty plot
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")

  if (has.title) {
    mtext(title, 
          cex = title.cex, side = 3, line = 0, adj = 0, font = 2, col = text.col) 
  }
  if (has.subtitle) {
    shift <- if (has.title) -1.2 else 0
    mtext(subtitle, 
          cex = cex, side = 3, line = shift, adj = 0, col = text.col) 
  }

  tind <- as.POSIXct(x[[1]])
  tnum <- as.numeric(tind)

  xlim <- range(tnum)
  ylim <- range(x[, value], na.rm = TRUE)

  xticks <- pretty(tind)
  xlabels <- format(xticks, "%Y")

  col <- colors_tsbox()[1:ts_nvar(x)]

  cnames <- ts_varnames(x)
  
  if (has.legend){
    legend("bottomleft", 
        legend = cnames, horiz = TRUE, 
        bty = "n", lty = 1, lwd = lwd, col = col, cex = cex, adj = 0, text.col = text.col)

  }

  # Second layer with graph
  # (that way, we can further process the graph later on)
  par(mar =  c(mar.b, mar.l , mar.t, 1.4), new = TRUE)

  # Main Plot
  plot(x = tind, type = "n",lty=1, pch=19, col=1,
      cex=1.5, lwd=1, las=1, bty="n", xaxt="n",
      xlim=xlim, ylim=ylim, xlab="", ylab=ylab,
      yaxt="n")

  axis(2, at=axTicks(2), labels=sprintf("%s", axTicks(2)),
      las=1, cex.axis=0.8, col=NA, line = -0.5, col.axis = axis.text.col)

  axis(side = 1, at = (xticks), 
       labels = xlabels, 
       las=1, cex.axis=0.8, col=NA, line = 0.5, tick = TRUE, padj = -2, col.axis = axis.text.col)

  # Gridlines
  abline(h = axTicks(2), v = xticks, col = "grey80", lty = "dotted", lwd = 0.5)

  # Lines
  all.vars <- ts_varnames(x)
  for (i in seq(all.vars)){
    vari <- all.vars[i]
    cd <- x[var == vari]
    cd <- cd[!is.na(value)]
    lines(y = cd[, value], 
        x = as.numeric(as.POSIXct(cd[[1]])), col = col[i], lwd = lwd)
  }

  cl <- match.call()
  assign("lastplot_call", cl, envir = .tsbox)

}



lastplot_call <- function(){
  get("lastplot_call", envir = .tsbox)
}


#' Save Previous Plot
#' 
#' @param ... additional arguments
#' @param open open
#' @param device device
#' @param height height
#' @param width width
#' @param filename filename
#' @export
ts_save <- function(filename = "myfig.pdf", width = 10, height = 5, device = "pdf", ..., open = TRUE){
  filename <- gsub(".pdf$", paste0(".", device), filename)

  cl <- lastplot_call()
  if (is.null(cl) || !inherits(cl, "call")){
    stop("ts_plot must be called first.")
  }

  if (device == "pdf"){
    pdf(file = filename,  width = width, height = height)
  } else if (device == "png"){
    png(filename = filename,  width = width, height = height, units = "in", res = 150)
  } else if (device == "bmp"){
    bmp(filename = filename,  width = width, height = height, units = "in", res = 150)
  } else if (device == "jpeg"){
    jpeg(filename = filename,  width = width, height = height, units = "in", res = 150)
  } else if (device == "tiff"){
    tiff(filename = filename,  width = width, height = height, units = "in", res = 150)
  } else {
    stop("device not supported.")
  }


  eval(cl, envir = parent.frame())
  dev.off()

  if (open) browseURL(filename)
}


