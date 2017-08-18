#' Plot muliple plots into a single plot. Optionally, it is possible that plots share a single legend (of first plot in provided)
#'
#' @title Multiplot
#' @param ...           Plots
#' @param plotlist      List of plots
#' @param shared_legend Set to true if plots should share the same legend
#' @param cols          Number of cols of the shared legend plot
#' @importFrom ggplot2 theme
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom grid unit unit.c
#' @export
multiplot <- function(..., plotlist = NULL, shared_legend = FALSE, cols = 1) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    if (shared_legend==TRUE) {
      g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom", legend.margin=unit(1, "cm")))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      grid.arrange(
        do.call(arrangeGrob, c(lapply(plots, function(x) x + theme(plot.margin = unit(x = c(0, 0, 1, 0), units = "cm")) + theme(legend.position="none")), ncol=cols)),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      )
    } else {
      grid.arrange(do.call(arrangeGrob, c(plots, ncol=cols)))
    }
  }
}

#' Theme for ggplot following APA guidelines
#'
#' @title APA Theme for ggplot
#' @param base_size base font size
#' @param base_family base font family
#' @return The theme
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect
#' @export
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg, colour = factor(gear))) + facet_wrap(~am)
#' p + theme_apa()
theme_apa <- function(base_size = 11, base_family = ""){
  half_line <- base_size/2
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(size = rel(1.2), hjust = 0.5, vjust = 1, face = "bold", colour = "black", margin = margin(b = half_line * 1.2)),
      axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1, colour = "black"),
      axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1, colour = "black"),
      strip.text.x = element_text(margin = margin(t = half_line, b = half_line), colour = "black"),
      complete = TRUE)
}

#' Plots the residuals of an ANOVA (aov) object for inspection
#'
#' @title Plot AOV Residuals
#' @param aov AOV object
#' @export
plotresiduals.aov <- function (aov) {
  par(mfrow=c(1,2))
  plot(fitted(aov[[4]]), studres(aov[[4]]))
  abline(h=0, lty=2)
  qqnorm(proj(aov)[[4]][, "Residuals"], ylab="Stratum 4 residuals")
  qqline(proj(aov)[[4]][, "Residuals"])
}