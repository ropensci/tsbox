.tsbox <- new.env(parent = emptyenv())

#' Plot Time Series
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param title title
#' @param subtitle subtitle
#' @examples
#' 
#' library(tsbox)
#' tsplot(AirPassengers, title = "Airline passengers", 
#'        subtitle = "The classic Box & Jenkins airline data")
#' tsplot(total = ldeaths, female = fdeaths, male = mdeaths)
#' 
#' tsplot(tsbind(sunspot.month, sunspot.year, lynx))
#' tsplot(tsscale(tsbind(airmiles, co2, JohnsonJohnson, discoveries)))
#' tsplot(EuStockMarkets)
#' tsplot(sunspot.month, sunspot.year, lynx)
#' tsplot(tsscale(tsbind(Nile, nottem, USAccDeaths)))
#' 
#' tsggplot(AirPassengers, title = "Airline passengers", 
#'        subtitle = "The classic Box & Jenkins airline data")
#' tsggplot(total = ldeaths, female = fdeaths, male = mdeaths)
#' 
#' tsggplot(tsbind(sunspot.month, sunspot.year, lynx))
#' tsggplot(tsscale(tsbind(airmiles, co2, JohnsonJohnson, discoveries)))
#' tsggplot(EuStockMarkets)
#' tsggplot(sunspot.month, sunspot.year, lynx)
#' tsggplot(tsscale(tsbind(Nile, nottem, USAccDeaths)))
#' \dontrun{
#' library(Quandl)
#' tsggplot(Quandl("FRED/GDPMC1", "xts"), title = "US GDP")
#' 
#' library(dataseries)
#' dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
#' tsggplot(tsscale(tswin(tsbind(`GDP Growth` = tspc(dta[, 'GDP.PBRTT.A.R']), 
#'                             `Consumer Sentiment Index` = dta[, 'CCI.CCIIR']), 
#'                      start = "1995-01-01")),
#'        title = "GDP and Consumer Sentiment",
#'        subtitle = "normalized values")
#' 
#' }
#' @export
#' @importFrom graphics abline axis axTicks legend lines mtext par plot
#' @importFrom grDevices dev.off pdf bmp jpeg png tiff
tsplot <- function(..., title, subtitle){
  x <- as_xts(tsbind(...))

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
  on.exit(par(op))

  lwd <- 1.5
  cex <- 0.9
  title.cex <- 1.2
  text.col <- "grey10"

  # c(bottom, left, top, right)
  if (NCOL(x) > 1){
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
  par(mar =  c(mar.b, 3, mar.t, 1.4))

  tind <- as.POSIXct(index(x))
  tnum <- as.numeric(tind)

  xlim <- range(tnum)
  ylim <- range(coredata(x), na.rm = TRUE)

  xticks <- pretty(tind)
  xlabels <- format(xticks, "%Y")

  col <- tscolors()[1:NCOL(coredata(x))]
  cnames <- colnames(coredata(x))

  # Main Plot
  plot(x = tind, type = "n",lty=1, pch=19, col=1,
      cex=1.5, lwd=1, las=1, bty="n", xaxt="n",
      xlim=xlim, ylim=ylim, xlab="", ylab="",
      yaxt="n")

  axis(2, at=axTicks(2), labels=sprintf("%s", axTicks(2)),
      las=1, cex.axis=0.8, col=NA, line = -0.5, col.axis = text.col)

  axis(side = 1, at = (xticks), 
       labels = xlabels, 
       las=1, cex.axis=0.8, col=NA, line = 0.5, tick = TRUE, padj = -2, col.axis = text.col)

  # Gridlines
  abline(h = axTicks(2), v = xticks, col = "grey50", lty = "dotted", lwd = 0.3)

  # Lines
  for (i in seq(NCOL(coredata(x)))){
    cd <- x[,i]
    cd <- cd[!is.na(cd)]
    lines(y = coredata(cd), 
        x = as.numeric(as.POSIXct(index(cd))), col = col[i], lwd = lwd)
  }

  # Second layer, with legend and title
  par(fig=c(0, 1, 0, 1), oma=c(0.5, 1, 2, 1), mar=c(0, 0, 0, 0), new=TRUE)

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

  if (has.legend){
    legend("bottomleft", 
        legend = cnames, horiz = TRUE, 
        bty = "n", lty = 1, lwd = lwd, col = col, cex = cex, adj = 0, text.col = text.col)

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
tssave <- function(filename = "myfig.pdf", width = 10, height = 5, device = "pdf", ..., open = TRUE){
  filename <- gsub(".pdf$", paste0(".", device), filename)

  cl <- lastplot_call()
  if (is.null(cl) || !inherits(cl, "call")){
    stop("tsplot must be called first.")
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


