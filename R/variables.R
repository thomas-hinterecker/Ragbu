
## Experiment tibbles
#' @importFrom tibble tibble
#' @export experiment.subsets
experiment.subsets <<- tibble(ID = numeric(), Name = character(), Subset = list())
#' @export experiment.results
experiment.results <<- tibble(ID = numeric(), Name = character(), Type = character(), Result = list())
#' @export experiment.plots
experiment.plots <<- tibble(ID = numeric(), Name = character(), Plot = list())

# Make Times New Roman font usable
if (Sys.info()["sysname"] == "Windows") {
  windowsFonts(RMN=windowsFont("Times New Roman"))
  #' @export RMN
  RMN <- "RMN"
} else {
  #' @export RMN
  RMN <- "Times New Roman"
}
