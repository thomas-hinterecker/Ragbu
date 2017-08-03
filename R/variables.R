
## Experiment tibbles
#' @importFrom tibble tibble
experiment.subsets <- tibble(ID = integer(), Name = character(), Subset = list())
experiment.results <- tibble(ID = integer(), Name = character(), Type = character(), Result = list())
experiment.plots <- tibble(ID = integer(), Name = character(), Plot = list())

experimentTibblesEnv <- new.env()
assign("experiment.subsets",experiment.subsets, envir=experimentTibblesEnv)
assign("experiment.results", experiment.results, envir=experimentTibblesEnv)
assign("experiment.plots", experiment.plots, envir=experimentTibblesEnv)

rm(experiment.subsets, experiment.results, experiment.plots)

# Make Times New Roman font usable
if (Sys.info()["sysname"] == "Windows") {
  windowsFonts(RMN=windowsFont("Times New Roman"))
  #' @export RMN
  RMN <- "RMN"
} else {
  #' @export RMN
  RMN <- "Times New Roman"
}
