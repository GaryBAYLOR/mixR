#' Plotting the Fitted Mixture Models
#'
#' This is the plot method for the class \code{mixfitEM}. It is used to plot the fitted mixture models
#' by using base R plotting system or using the package ggplot2.
#'
#' The function \code{plot.mixfitEM} is used for plotting an object of class \code{mixfitEM}, which is
#' an output of the function \code{\link{mixfit}}. Users can choose base R plotting system or ggplot2
#' (the package ggplot2 needs to be installed).
#' plotting system. The plot is a density plot of the fitted mixture model imposed on top of a histogram.
#' The parameters that control the appearance of the histogram and the density curve can be changed.
#' The density curve of each component can be shown or hidden.
#'
#' @param x an object of class \code{mixfitEM}, an output from the function \code{\link{mixfit}}
#' @param theme a string the specifies the appearance of the plot, which is from the ggplot2 and could
#' be one of'gray', 'bw' (default), 'linedraw', 'light', 'dark', 'minimal', 'classic', or 'void'.
#' @param add_hist a logical value specifying whether a histogram of data should be plotted
#' @param add_poly a logical value specifying whether a polygon of each component should be plotted.
#' @param add_legend a logical value specifying whether the legend should be plotted.
#' @param smoothness a positive integer controlling the smoothness of the density curve in the plot.
#' The default value is 512 and increasing this value will produce smoother curve.
#' @param trans the transparency of the polygons if they are plotted (default 0.5)
#' @param cut the number of standard deviations from the center of each component we want to plot 
#' the density (default 3.8)
#' @param xlab the label for x axis
#' @param ylab the label for y axis
#' @param title the title of the plot
#' @param breaks the number of bins used for plotting the histogram
#' @param plot.title an object returned by element_text() to specify the appearance of the title
#' @param axis.text.x an object returned by element_text() to specify the appearance of the x axis
#' @param axis.text.y an object returned by element_text() to specify the appearance of the y axis
#' @param axis.title.x an object returned by element_text() to specify the appearance of the label along x axis
#' @param axis.title.y an object returned by element_text() to specify the appearance of the label along y axis
#' @param legend.title an object returned by element_text() to specify the appearance of the legend title
#' @param legend.text an object returned by element_text() to specify the appearance of the legend text
#' @param legend.position the position of the legend, could be 'right'(default), 'right', 'top', or 'bottom'
#' @param legend.direction the direction of the legend, could be 'vertical' (default) or 'horizontal'
#' @param ... other arguments
#' 
#' @seealso \code{\link{mixfit}}
#'
#' @examples
#' x <- rmixnormal(200, c(0.3, 0.7), c(2, 5), c(1, 0.7))
#' mod <- mixfit(x, ncomp = 2)
#' plot(mod)
#' plot(mod, theme = 'classic') 
#'
#' @export
#' @import ggplot2
plot.mixfitEM = function(x, 
                         theme = NULL, 
                         add_hist = TRUE, add_poly = TRUE, add_legend = TRUE,
                         smoothness = 512, 
                         trans = 0.5,
                         cut = 3.8,
                         xlab, ylab,
                         title,
                         breaks,
                         plot.title = element_text(hjust = 0.5),
                         axis.text.x = element_text(),
                         axis.text.y = element_text(),
                         axis.title.x = element_text(),
                         axis.title.y = element_text(),
                         legend.title = element_text(),
                         legend.text = element_text(),
                         legend.position = 'right',
                         legend.direction = ifelse(legend.position %in% c('top', 'bottom'), 'horizontal', 'vertical'),
                         ...
) {
  if(is.null(theme)) theme = 'bw'
  if(!theme %in% tolower(c('gray', 'bw', 'linedraw', 'light', 'dark', 'minimal', 'classic', 'void'))) {
    stop("'theme' must be one of 'gray', 'bw', 'linedraw', 'light', 'dark', 'minimal', 'classic', 'void'.")
  }
  
  family <- x$family
  ncomp <- length(x$pi)
  data = x$data
  
  # calculate density of the plotting object
  d = density(x, smoothness = smoothness, cut = cut)
  d$comp[is.infinite((d$comp))] = 0
  d$y[is.infinite(d$y)] = 0
  
  # prepare data for plotting, 'breaks' is used to plot histogram
  if(is.matrix(data)) {
    if(missing(breaks)) {
      breaks <- sort(unique(c(data[, 1], data[, 2])))
    } else {
      breaks <- seq(min(data[, 1:2]), max(data[, 1:2]), length = breaks + 1)
    }
    data <- reinstate(data)
  } else {
    if(missing(breaks)) {
      breaks <- 30
    }
    breaks <- seq(min(data), max(data), length = breaks + 1)
  }
  
  # set xlim
  range = max(d$x) - min(d$x)
  xlim = c(min(d$x) - range * 0.05, max(d$x) + range * 0.05)
  
  # set ylim
  tmp <- bin(data, brks = breaks)
  count <- tmp[, 3]
  max_freq <- max(count) / (sum(count) * (breaks[2] - breaks[1]))
  ylim <- c(0, max(c(d$y, max_freq * 1.05)))

  
  # set xlab, ylab, title
  if(missing(xlab)) {
    xlab <- "Data"
  }
  if(missing(ylab)) {
    ylab <- "Density"
  }
  if(missing(title)) {
    title = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                 paste(ifelse(family == 'lnorm', "Log-normal", family),"Mixture Density"),
                 perl = TRUE)
  }
  
  # add histogram
  if(add_hist) {
    add_hist <- geom_histogram(aes(x = data, y = after_stat(density)), breaks = breaks, color = "black",
                               fill = "white", size = 0.3) 
  } else {
    add_hist = NULL
  }
  
  # add polygon
  if(add_poly) {
    df_poly <- data.frame(x = rep(d$x, ncomp), 
                          comp = rep(1:ncomp, each = smoothness),
                          y = as.vector(d$comp))
    
    add_poly <- geom_polygon(data = df_poly, aes(x, y, fill = as.factor(comp)), alpha = trans)
  } else {
    add_poly = NULL
  }
  
  # add legend
  if(!add_legend) {
    remove_legend <- theme(legend.position = "none")
  } else {
    remove_legend <- NULL
  }
  
  # plotting style
  style <- switch(theme, 
                  gray = theme_gray(),
                  bw = theme_bw(),
                  linedraw = theme_linedraw(),
                  light = theme_light(),
                  dark = theme_dark(),
                  minimal = theme_minimal(),
                  classic = theme_classic(),
                  void = theme_void())
  
  # ggplot
  ggplot(as.data.frame(data)) + add_hist + add_poly + style +
    geom_path(data = data.frame(x = d$x, y = d$y), aes(x, y)) +
    scale_fill_discrete(guide = guide_legend(title = "Comp")) + remove_legend +
    labs(title = title, x = xlab, y = ylab) +
    theme(plot.title = plot.title,
          axis.text.x = axis.text.x,
          axis.text.y = axis.text.y,
          axis.title.x = axis.title.x,
          axis.title.y = axis.title.y,
          legend.title = legend.title,
          legend.text = legend.text,
          legend.position = legend.position,
          legend.direction = legend.direction) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)
}
