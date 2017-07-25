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
#' @param plot.box             Is your plot a boxplot? (boolean)
#' @param font_size_multiplier By how much do you want to muliply for font size? (Standard is 1)
#' @return The theme
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect
#' @export
theme_apa <- function(plot.box = FALSE, base_size = 20){
  if (Sys.info()["sysname"] != "Windows") {
    windowsFonts <- NULL
  }
  if (Sys.info()["sysname"] == "Windows") {
    windowsFonts(RMN=windowsFont("Times New Roman"))
    RMN <- "RMN"
  } else {
    RMN <- "Times New Roman"
  }
  out <- theme(
    plot.title=element_text(family=RMN, size=base_size, hjust=0.5, face="bold", colour="black", margin=margin(t=0, r=0, b=20, l=0)),
    legend.text=element_text(family=RMN, size=0.9*base_size),
    legend.title=element_text(family=RMN, size=20*base_size),
    axis.title.x=element_text(family=RMN, size=base_size, margin=margin(t=15, r=0, b=5, l=0), colour="black"),
    axis.title.y=element_text(family=RMN, size=base_size, margin=margin(t=0, r=15, b=0, l=0), angle=90, colour="black"),
    axis.text.x=element_text(family=RMN, size=0.9*base_size, colour="black"),
    axis.text.y=element_text(family=RMN, size=0.9*base_size, colour="black"),
    strip.text.x=element_text(family=RMN, size=0.9*base_size, face="bold"),
    strip.background=element_blank(),
    axis.ticks=element_line(colour="black")
  )
  if (!plot.box) {
    # The plot is not a boxplot
    out <- out + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, colour = "white")) +
      theme(axis.line.x = element_line(), axis.line.y = element_line())
  } else {
    # The plot is a boxplot
    out <- out + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, colour = "grey50")
    )
  }
  out
}