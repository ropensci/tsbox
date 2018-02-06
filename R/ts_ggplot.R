#' ggplot Theme for tsbox
#' 
#' Using same style as `ts_plot()`.
#' 
#' @param base_family base font family
#' @param base_size base font size
#' @param ... aruments passed to subfunctions
#' @examples
#' 
#' df <- ts_df(ts_c(total = ldeaths, female = fdeaths, male = mdeaths))
#' # standard ggplot
#' library(ggplot2)
#' ggplot(df, aes(x = time, y = value, color = id)) + 
#'   geom_line()
#' 
#' # a quick plot for time series
#' p <- ts_ggplot(AirPassengers, mdeaths)
#' 
#' # using same style as base graphic ts_plot()
#' p +
#'   theme_tsbox() + 
#'   scale_color_tsbox()
#' 
#' @export
theme_tsbox <- function(base_family = getOption("ts_font", ""), base_size = 12){
  # 'Source Sans Pro'  # does not work on mac
  # 'Slabo 13px'

  margin <- NULL 

  half_line <- base_size/2
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
  ggplot2::theme(
        
        # line = ggplot2::element_line(color = "grey30", size = 0.4),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(size = ggplot2::rel(0.9), color = "grey10",
                                             margin = margin(t = 0, r = 7, b = 0, l = 0)
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

# #' @export
# #' @rdname theme_tsbox
# theme_tsbox_scatter <- function(){
#   theme_tsbox() +
#   theme(axis.title.x = element_text(margin = margin(8, 0, 0, 0), size = 10),
#         axis.title.y = element_text(margin = margin(0, 8, 0, 0), angle = 90, size = 10),
#         panel.grid.major.x = element_line(colour = "black"), 
#         axis.line.x =  element_blank(),
#         axis.ticks.x = element_blank()
#     )
# }


#' @export
#' @rdname theme_tsbox
colors_tsbox <- function(){
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
#' @rdname theme_tsbox
scale_color_tsbox <- function(...) {
    stopifnot(requireNamespace("ggplot2"))
    ggplot2::discrete_scale("colour", "ds", scales::manual_pal(colors_tsbox()), ...)
}

#' @export
#' @rdname theme_tsbox
scale_fill_tsbox <- function (...) {
    stopifnot(requireNamespace("ggplot2"))
    ggplot2::discrete_scale("fill", "ds", scales::manual_pal(colors_tsbox()), ...)
}



#' @rdname ts_plot
#' @export
ts_ggplot <- function (...) {

  # TODO add some multi dim support

  
  stopifnot(requireNamespace("ggplot2"))
  x <- ts_dts(ts_c(...))

  colname.time <- colname_time(x)
  colname.value <- colname_value(x)
  colname.id <- colname_id(x)

  df <- ts_df(x)
  df <- df[!is.na(df[, colname.value]), ]

  n <- NCOL(df)
  stopifnot(n > 1)
  if (n == 2){
    p <- ggplot2::ggplot(df,  ggplot2::aes_string(x = colname.time, y = colname.value)) 
  } else if (n > 2){

    # numeric variable 'levels'
    if (class(df[[colname.id]]) %in% c("integer", "numeric")){
      df[[colname.id]] <- as.character(df[[colname.id]])
    }

    if (length(unique(df[[colname.id]])) > 29) {
      stop(length(unique(df[[colname.id]])), " time series supplied. Maximum is 29.",  call. = FALSE)
    }
    p <-  ggplot2::ggplot(df,  ggplot2::aes_string(x = colname.time, y = colname.value, color = colname.id))
  } 
  p <- p + ggplot2::geom_line() 

  p

}



