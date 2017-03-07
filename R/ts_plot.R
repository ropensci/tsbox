#' ggplot theme for dataseries.org
#' 
#' @examples
#' library(tsbox)
#' df <- as_df(cbind(total = ldeaths, female = fdeaths, male = mdeaths))
#' 
#' ggplot(df, aes(x = Index, y = Value, color = Series)) + 
#'   geom_line() +
#'   ggtitle('Deaths by lung diseases', subtitle = 'United Kindom, per year') + 
#'   theme_ts() + 
#'   scale_color_ts() 
#' 
#' ggsave("myfig.pdf", width = 8, height = 5)
#' browseURL("myfig.pdf")
#' 
#' 
#' ### a non time series example (from ggplot help)
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
#' ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))
#' ggplot() +
#'   geom_point(data = df, aes(x = gp, y = y)) +
#'   geom_point(data = ds, aes(x = gp, y = mean), size = 3, color = ts_colors()[2]) +
#'   geom_errorbar(data = ds, aes(x = gp, y = mean,
#'                     ymin = mean - sd, ymax = mean + sd), , color = ts_colors()[3], width = 0.1) +
#'   theme_ts() + ggtitle("Prettifying ggplot", subtitle = "with the dataseries.org theme")
#' 
#' ### and a real scatter plot
#' ggplot(mtcars, aes(x=wt, y=mpg, color = as.factor(gear))) + geom_point(size = 3) + 
#'   theme_ts_scatter() + scale_color_ts() + 
#'   ggtitle("Drink and drive", subtitle = "fuel consumption and automobile design") + 
#'   xlab("weight (1000 lbs)") + ylab("miles/(US) gallon") +
#'   theme(legend.title = element_text()) + labs(color = "Gears")
#'   
#' ### with additional geoms
#' p <- qplot(rnorm(200), rnorm(200)) +
#'   theme_ts_scatter() + 
#'   ggtitle("Drink and drive", subtitle = "fuel consumption and automobile design") + 
#'   xlab("weight (1000 lbs)") + ylab("miles/(US) gallon") + geom_smooth(color = ts_colors()[2])
#' 
#' ### and bar plot
#' dat1 <- data.frame(
#'     sex = factor(c("Female","Female","Male","Male")),
#'     time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
#'     total_bill = c(13.53, 16.81, 16.24, 17.42)
#' )
#' ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
#'     geom_bar(stat="identity", position=position_dodge(), width=0.4) + theme_ts() + scale_fill_ts() + 
#'     ggtitle("Battle of the sexes", subtitle = "not sure what the data is about")
#' 
#'     
#' @import ggplot2 extrafont
#' @export
theme_ts <- function(base_family = getOption("ts_font", "sans")){
  # 'Source Sans Pro'  # does not work on mac
  # 'Slabo 13px'
  theme_minimal(base_family=base_family) +
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

#' @export
#' @rdname theme_ts
theme_ts_scatter <- function(){
  theme_ts() +
  theme(axis.title.x = element_text(margin = margin(8, 0, 0, 0), size = 10),
        axis.title.y = element_text(margin = margin(0, 8, 0, 0), angle = 90, size = 10),
        panel.grid.major.x = element_line(colour = "black"), 
        axis.line.x =  element_blank(),
        axis.ticks.x = element_blank()
    )
}


#' @export
#' @rdname theme_ts
ts_colors <- function(){
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
#' @rdname theme_ts
scale_color_ts <- function(...) {
    discrete_scale("colour", "ds", scales::manual_pal(ts_colors()), ...)
}

#' @export
#' @rdname theme_ts
scale_fill_ts <- function (...) {
    discrete_scale("fill", "ds", scales::manual_pal(ts_colors()), ...)
}



#' Beautiful Graphics
#' 
#' @examples
#' library(mytools)
#' ts_plot(AirPassengers) + ggtitle("Airline passengers", subtitle = "The classic Box & Jenkins airline data")
#' ts_plot(cbind(total = ldeaths, female = fdeaths, male = mdeaths))
#' 
#' library(Quandl)
#' ts_plot(Quandl("FRED/GDPMC1", "xts"))
#' ggsave("myfig.pdf", width = 8, height = 5)
#' @export
ts_plot <- function (x, title = NULL, subtitle = NULL, ...) UseMethod("ts_plot")

#' @export
#' @method ts_plot numeric
ts_plot.numeric <- function(x, title = NULL, subtitle = NULL, ...){
  ts_plot(ts(x), title = title, subtitle = subtitle, ...)
}


#' @export
#' @method ts_plot ts
ts_plot.ts <- function(x, title = NULL, subtitle = NULL, ...){
  df <- as_df(x)
  ts_plot_core(df, title = title, subtitle = subtitle, ...)
}
  
#' @export
#' @method ts_plot xts
ts_plot.xts <- function(x, title = NULL, subtitle = NULL, ...){
  df <- as_df(x)
  ts_plot_core(df, title = title, subtitle = subtitle, ...)
}

ts_plot_core <- function(df, title = NULL, subtitle = NULL, ...){
  n <- NCOL(df)
  if (n == 2){
    p <- ggplot(df, aes(x = Index, y = Value)) 
  } else if (n == 3){
    p <- ggplot(df, aes(x = Index, y = Value, color = Series)) 
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

#' @export
ts_ggsave <- function(filename = "myfig.pdf", width = 8, height = 5, ...){
  ggsave(filename = filename, width = width, height = height, ...)
  browseURL(filename)
}




