#' ggplot Theme for tsbox
#' 
#' @param base_family base font family
#' @param base_size base font size
#' @param ... aruments passed to subfunctions
#' @examples
#' library(tsbox)
#' df <- as_df(tsbind(total = ldeaths, female = fdeaths, male = mdeaths))
#'  \dontrun{
#' ggplot(df, aes(x = Index, y = Value, color = Series)) + 
#'   geom_line() +
#'   ggtitle('Deaths by lung diseases', subtitle = 'United Kindom, per year') + 
#'   theme_ts() + 
#'   scale_color_ts() 
#' 
#' ggsave("myfig.pdf", width = 8, height = 5)
#' browseURL("myfig.pdf")
#' }
#' @import ggplot2
#' @export
theme_ts <- function(base_family = getOption("ts_font", ""), base_size = 11){
  # 'Source Sans Pro'  # does not work on mac
  # 'Slabo 13px'

  half_line <- base_size/2
  theme_minimal(base_family = base_family, base_size = base_size) +
  theme(
        
        # line = element_line(color = "grey30", size = 0.4),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "grey30", face = "bold", margin = margin(t = half_line * 2, b = half_line * 0.7), hjust = 0, size = rel(1.2)),
        plot.subtitle = element_text(color = "grey50", margin = margin(t = 0, b = half_line * 1.2), size = rel(0.9), hjust = 0),
        plot.caption = element_text(color = "grey50", margin = margin(t = 0, b = half_line * 1.2), size = rel(0.8)),

        # panel.grid = element_line(colour = NULL, linetype = 3), 
        # panel.grid.major = element_line(colour = "grey30"), 
        panel.grid = element_line(size = 0.2), 
        # panel.grid.major.x = element_blank(), 
        # panel.grid.minor = element_blank(),

        axis.text = element_text(color = "grey50", size = rel(0.7)),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey50", size = rel(0.9)),
        legend.position = "bottom",
        legend.direction = "horizontal"

        # axis.ticks.x = element_line(color = "grey30"),
        # axis.ticks.y = element_blank(),
        # panel.border = element_blank(),
        # axis.line.x =  element_line()
    )
}

# #' @export
# #' @rdname theme_ts
# theme_ts_scatter <- function(){
#   theme_ts() +
#   theme(axis.title.x = element_text(margin = margin(8, 0, 0, 0), size = 10),
#         axis.title.y = element_text(margin = margin(0, 8, 0, 0), angle = 90, size = 10),
#         panel.grid.major.x = element_line(colour = "black"), 
#         axis.line.x =  element_blank(),
#         axis.ticks.x = element_blank()
#     )
# }


#' @export
#' @rdname theme_ts
tscolors <- function(){
      c(
        "#4D4D4D",
        "#5DA5DA",
        "#FAA43A",
        "#60BD68",
        "#F15854",
        "#B276B2",
        "#DECF3F",
        "#F17CB0",
        "#B2912F"
      )
}

#' @export
#' @import scales
#' @rdname theme_ts
scale_color_ts <- function(...) {
    discrete_scale("colour", "ds", scales::manual_pal(tscolors()), ...)
}

#' @export
#' @rdname theme_ts
scale_fill_ts <- function (...) {
    discrete_scale("fill", "ds", scales::manual_pal(tscolors()), ...)
}



#' Plot Time Series
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param title title
#' @param subtitle subtitle
#' @examples
#' \dontrun{
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
#' library(Quandl)
#' tsplot(Quandl("FRED/GDPMC1", "xts"), title = "US GDP")
#' 
#' library(dataseries)
#' dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
#' tsplot(tsscale(tswin(tsbind(`GDP Growth` = tspc(dta[, 'GDP.PBRTT.A.R']), 
#'                             `Consumer Sentiment Index` = dta[, 'CCI.CCIIR']), 
#'                      start = "1995-01-01")),
#'        title = "GDP and Consumer Sentiment",
#'        subtitle = "normalized values")
#' 
#' }
#' @export
tsplot <- function (..., title = NULL, subtitle = NULL) UseMethod("tsplot")

#' @export
#' @rdname tsplot
#' @method tsplot numeric
tsplot.numeric <- function(..., title = NULL, subtitle = NULL){
  x <- tsbind(...)
  tsplot(ts(x), title = title, subtitle = subtitle)
}

#' @export
#' @rdname tsplot
#' @method tsplot ts
tsplot.ts <- function(..., title = NULL, subtitle = NULL){
  df <- as_data.frame(tsbind(...))
  tsplot_core(df, title = title, subtitle = subtitle)
}
  
#' @export
#' @rdname tsplot
#' @method tsplot xts
tsplot.xts <- function(..., title = NULL, subtitle = NULL){
  df <- as_data.frame(tsbind(...))
  tsplot_core(df, title = title, subtitle = subtitle)
}

#' @export
#' @rdname tsplot
#' @method tsplot data.frame
tsplot.data.frame <- function(..., title = NULL, subtitle = NULL){
  x <- tsbind(...)
  tsplot_core(x, title = title, subtitle = subtitle)
}

#' @export
#' @rdname tsplot
#' @method tsplot data.table
tsplot.data.table <- function(..., title = NULL, subtitle = NULL){
  x <- tsbind(...)
  tsplot_core(x, title = title, subtitle = subtitle)
}

tsplot_core <- function(df, title = NULL, subtitle = NULL, ...){
  df <- df[!is.na(df[, 'value']), ]
  n <- NCOL(df)
  if (n == 2){
    p <- ggplot(df, aes_string(x = 'time', y = 'value')) 
  } else if (n == 3){
    p <- ggplot(df, aes_string(x = 'time', y = 'value', color = 'variable'))
  } else {
    stop("df has wrong col dim")
  }
  p <- p + 
  geom_line() +
  ylab("") + 
  theme_ts(...) + 
  scale_color_ts() 

  if (!is.null(title) | !is.null(subtitle)){
    if (is.null(title)) title <- ""  # subtitle only
    p <- p + ggtitle(label = title, subtitle = subtitle)
  }
  p
}


#' ggsave, optimized for time series
#' 
#' @param filename filename
#' @param width width
#' @param height height
#' @param device device
#' @param ... aruments passed to ggsave
#' @param open should the graph be opened?
#' @examples
#' \dontrun{
#' tsplot(AirPassengers)
#' tssave()
#' }
#' @import ggplot2
#' @export
tssave <- function(filename = "myfig.pdf", width = 7, height = 4, device = "pdf", ..., open = TRUE){
  filename <- gsub(".pdf$", paste0(".", device), filename)
  ggsave(filename = filename, width = width, height = height, device = device, ...)

  if (open) browseURL(filename)
}




