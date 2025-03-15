# This function is adapted from ggpubr::ggscatterhist
# Original ggpubr package by Alboukadel Kassambara (2020)
# https://rpkgs.datanovia.com/ggpubr/
# Modifications made to customize margin plots and enhance flexibility.
myggscatterhist <- function (data, x, y, group = NULL, color = "black", fill = NA, 
          palette = NULL, shape = 19, size = 2, linetype = "solid", 
          binwidth = 30, margin.plot = c("density", "histogram", "boxplot"), 
          margin.params = list(), margin.ggtheme = theme_void(), margin.space = FALSE, 
          main.plot.size = 2, margin.plot.size = 1, title = NULL, 
          xlab = NULL, ylab = NULL, legend = "top", ggtheme = theme_pubr(), 
          print = TRUE, hist_alpha = .7, ...) 
{
  if (!ggpubr:::has_cowplot_v0.9()) {
    warning("Install the latest developmental version of cowplot on github ", 
            "to fully use all the feature of ggscatterhist", 
            .call = FALSE)
  }
  margin.plot <- match.arg(margin.plot)
  margin.params <- ggpubr:::.check_margin_params(margin.params, data, 
                                        color, fill, linetype)
  if (margin.plot == "histogram") {
    if (is.null(margin.params$position)) {
      margin.params$position <- "identity"
    }
    margin.params <- margin.params %>% ggpubr:::.add_item(binwidth = binwidth)
  }
  if (!is.null(group)) {
    if (missing(color)) 
      color = group
    if (missing(shape)) 
      shape = group
  }
  . <- NULL
  sp <- ggpubr:::ggscatter(data, x, y, color = color, fill = fill, 
                  palette = palette, shape = shape, size = size, xlab = xlab, 
                  ylab = ylab, ggtheme = ggtheme, title = title, legend = legend, 
                  ...)
  geomfunc <- switch(margin.plot, histogram = geom_histogram, 
                     density = geom_density, boxplot = geom_boxplot, geom_histogram)
  if (margin.plot %in% c("density", "histogram")) {
    xplot.x <- x
    xplot.y <- NULL
    yplot.x <- y
    yplot.y <- NULL
  }
  else if (margin.plot %in% c("boxplot")) {
    if (is.null(group)) {
      data <- data %>% mutate(.xgroupx. = factor(1))
      group = ".xgroupx."
    }
    xplot.x <- group
    xplot.y <- x
    yplot.x <- group
    yplot.y <- y
  }
  xplot <- ggplot() + margin.params %>% ggpubr:::.add_item(geomfunc = geomfunc, 
                                                  data = data, 
                                                  x = xplot.x, 
                                                  y = xplot.y, 
                                                  alpha = hist_alpha) %>% 
    do.call(geom_exec, .)
  xplot <- ggpubr:::set_palette(xplot, palette)
  yplot <- ggplot() + margin.params %>% ggpubr:::.add_item(geomfunc = geomfunc, 
                                                  data = data, 
                                                  x = yplot.x, 
                                                  y = yplot.y, 
                                                  alpha = hist_alpha) %>% 
    do.call(geom_exec, .)
  yplot <- ggpubr:::set_palette(yplot, palette)
  if (margin.plot %in% c("density", "histogram")) 
    yplot <- yplot + coord_flip()
  else if (margin.plot %in% c("boxplot")) 
    xplot <- xplot + coord_flip()
  .legend <- ggpubr:::get_legend(sp)
  sp <- sp + theme(plot.margin = grid::unit(c(0, 0, 0.25, 
                                              0.25), "cm"))
  xplot <- xplot + margin.ggtheme + ggpubr:::clean_theme() + ggpubr:::rremove("legend") + 
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))
  yplot <- yplot + margin.ggtheme + ggpubr:::clean_theme() + ggpubr:::rremove("legend") + 
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))
  plots <- list(sp = sp, xplot = xplot, yplot = yplot)
  class(plots) <- c("ggscatterhist", "list")
  if (print) {
    res <- print(plots, margin.space = margin.space, main.plot.size = main.plot.size, 
                 margin.plot.size = margin.plot.size, title = title, 
                 legend = legend)
  }
  invisible(plots)
}
