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
        plot.title = element_text(color = "grey10", face = "bold", margin = margin(t = half_line * 2, b = half_line * 0.7), hjust = 0, size = rel(1.2)),
        plot.subtitle = element_text(color = "grey10", margin = margin(t = 0, b = half_line * 1.2), size = rel(0.9), hjust = 0),
        plot.caption = element_text(color = "grey50", margin = margin(t = 0, b = half_line * 1.2), size = rel(0.8)),

        # panel.grid = element_line(colour = NULL, linetype = 3), 
        # panel.grid.major = element_line(colour = "grey10"), 
        panel.grid = element_line(size = 0.2), 
        # panel.grid.major.x = element_blank(), 
        # panel.grid.minor = element_blank(),

        axis.text = element_text(color = "grey10", size = rel(0.7)),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey10", size = rel(0.9)),
        legend.position = "bottom",
        legend.direction = "horizontal"

        # axis.ticks.x = element_line(color = "grey10"),
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
"#B2912F",
"#4afff0", "#34bdcc", "#4f61a1", "#461e78", "#440a4f", "#c3fbc4",
"#85f9d6", "#79c7ad", "#a6cc7a", "#dfff7b",
"#8d7b88", "#4e414f", "#baadb5", "#2d2538", "#837a80", "#fff68f",
"#800080", "#f8b1cc", "#c29bff", "#8d0808"
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



#' @rdname tsplot
#' @export
tsggplot <- function (..., title = NULL, subtitle = NULL) UseMethod("tsggplot")

#' @export
#' @rdname tsplot
#' @method tsggplot numeric
tsggplot.numeric <- function(..., title = NULL, subtitle = NULL){
  x <- tsbind(...)
  tsggplot(ts(x), title = title, subtitle = subtitle)
}

#' @export
#' @rdname tsplot
#' @method tsggplot ts
tsggplot.ts <- function(..., title = NULL, subtitle = NULL){
  df <- as_data.frame(tsbind(...))
  tsggplot_core(df, title = title, subtitle = subtitle)
}
  
#' @export
#' @rdname tsplot
#' @method tsggplot xts
tsggplot.xts <- function(..., title = NULL, subtitle = NULL){
  df <- as_data.frame(tsbind(...))
  tsggplot_core(df, title = title, subtitle = subtitle)
}

#' @export
#' @rdname tsplot
#' @method tsggplot data.frame
tsggplot.data.frame <- function(..., title = NULL, subtitle = NULL){
  x <- as_data.frame(tsbind(...))
  tsggplot_core(x, title = title, subtitle = subtitle)
}

#' @export
#' @rdname tsplot
#' @method tsggplot data.table
tsggplot.data.table <- function(..., title = NULL, subtitle = NULL){

  # a bit a mystery that as_data.frame.data.table is not working...
  x <- as_data.frame(tsbind(...))  

  tsggplot_core(x, title = title, subtitle = subtitle)
}

tsggplot_core <- function(df, title = NULL, subtitle = NULL){
  df <- df[!is.na(df[, 'value']), ]



  n <- NCOL(df)
  if (n == 2){
    p <- ggplot(df, aes_string(x = 'time', y = 'value')) 
  } else if (n == 3){

    # numeric variable 'levels'
    if (class(df$variable) %in% c("integer", "numeric")){
      df[,'variable'] <- as.character(df[,'variable'])
    }

    if (length(unique(df[,'variable'])) > 29) {
      stop(length(unique(df[,'variable'])), " time series supplied. Maximum is 29.",  call. = FALSE)
    }

    p <- ggplot(df, aes_string(x = 'time', y = 'value', color = 'variable'))
  } else {
    stop("df has wrong col dim")
  }
  p <- p + 
  geom_line() +
  ylab("") + 
  theme_ts() + 
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
#' tsggplot(AirPassengers)
#' tssave()
#' }
#' @import ggplot2
#' @export
tsggsave <- function(filename = "myfig.pdf", width = 10, height = 5, device = "pdf", ..., open = TRUE){
  filename <- gsub(".pdf$", paste0(".", device), filename)
  ggsave(filename = filename, width = width, height = height, device = device, ...)

  if (open) browseURL(filename)
}




