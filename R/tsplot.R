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
theme_ts <- function(base_family = getOption("ts_font", "sans"), base_size = ""){
  # 'Source Sans Pro'  # does not work on mac
  # 'Slabo 13px'
  theme_minimal(base_family = base_family, base_size = base_size) +
  theme(
        # line = element_line(color = "grey30", size = 0.4),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(margin = margin(10, 0, 7, 0), hjust = 0, size = 14),
        plot.subtitle = element_text(margin = margin(0, 0, 9, 0), hjust = 0),
        # panel.grid = element_line(colour = NULL, linetype = 3), 
        # panel.grid.major = element_line(colour = "grey30"), 
        panel.grid = element_line(size = 0.2), 
        # panel.grid.major.x = element_blank(), 
        # panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
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
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param title title
#' @param subtitle subtitle
#' @param ... addtiional arguments, passed to ggplot.
#' @examples
#' \dontrun{
#' library(tsbox)
#' tsplot(AirPassengers) + 
#'   ggtitle("Airline passengers", 
#'            subtitle = "The classic Box & Jenkins airline data")
#' tsplot(tsbind(total = ldeaths, female = fdeaths, male = mdeaths))
#' 
#' 
#' library(Quandl)
#' tsplot(Quandl("FRED/GDPMC1", "xts"))
#' ggsave("myfig.pdf", width = 8, height = 5)
#' }
#' @export
tsplot <- function (x, title = NULL, subtitle = NULL, ...) UseMethod("tsplot")

#' @export
#' @rdname tsplot
#' @method tsplot numeric
tsplot.numeric <- function(x, title = NULL, subtitle = NULL, ...){
  tsplot(ts(x), title = title, subtitle = subtitle, ...)
}

#' @export
#' @rdname tsplot
#' @method tsplot ts
tsplot.ts <- function(x, title = NULL, subtitle = NULL, ...){
  df <- as_data.frame(x)
  tsplot_core(df, title = title, subtitle = subtitle, ...)
}
  
#' @export
#' @rdname tsplot
#' @method tsplot xts
tsplot.xts <- function(x, title = NULL, subtitle = NULL, ...){
  df <- as_data.frame(x)
  tsplot_core(df, title = title, subtitle = subtitle, ...)
}

#' @export
#' @rdname tsplot
#' @method tsplot data.frame
tsplot.data.frame <- function(x, title = NULL, subtitle = NULL, ...){
  tsplot_core(x, title = title, subtitle = subtitle, ...)
}

#' @export
#' @rdname tsplot
#' @method tsplot data.table
tsplot.data.table <- function(x, title = NULL, subtitle = NULL, ...){
  tsplot_core(x, title = title, subtitle = subtitle, ...)
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
#' @param open should the graph be opened?
#' @param ... aruments passed to ggsave
#' @examples
#' \dontrun{
#' tsplot(AirPassengers)
#' tssave()
#' }
#' @import ggplot2
#' @export
tssave <- function(filename = "myfig.pdf", width = 8, height = 5, ..., open = TRUE){
  ggsave(filename = filename, width = width, height = height, ...)

  if (open) browseURL(filename)
}




