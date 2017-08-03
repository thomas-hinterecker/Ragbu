#' Add a tibble or data frame to the experiment data subsets
#'
#' @title Add to Data Subsets
#' @param subset Data
#' @param name Name of the subset
#' @param id Subset id (optional)
#' @return The subset
#' @export
subsets.add <- function (subset, name, id=0) {
  experiment.subsets <- get("experiment.subsets", envir=experimentTibblesEnv)
  subset_list = list(subset)
  if (id > 0 && any(with(experiment.subsets, ID == id))) {
    experiment.subsets <- mutate(
      experiment.subsets,
      Subset = ifelse(ID == id, subset_list, Subset))
  } else if (any(with(experiment.subsets, Name == name))) {
    experiment.subsets <- mutate(
      experiment.subsets,
      Subset = ifelse(Name == name, subset_list, Subset))
  } else {
    if (id == 0) {
      if (nrow(experiment.subsets) > 0) { id <- max(experiment.subsets$ID) + 1 } else { id <- 1 }
    }
    experiment.subsets <- add_row(
      experiment.subsets,
      ID = id,
      Name = name,
      Subset = subset_list)
  }
  assign("experiment.subsets", experiment.subsets, envir=experimentTibblesEnv)
  return(subset)
}

#' Get a subset from the experiment data subsets
#'
#' @title Get Subset
#' @param id The subset id or name
#' @return The subset
#' @export
subsets.get <- function (id) {
  experiment.subsets <- get("experiment.subsets", envir=experimentTibblesEnv)
  if (is.numeric(id)) {
    return((filter(experiment.subsets, ID == id))$Subset[[1]])
  } else {
    return((filter(experiment.subsets, Name == id))$Subset[[1]])
  }
}

#' Lists all the stored subsets
#'
#' @title List Subsets
#' @export
subsets.list <- function () {
  experiment.subsets <- get("experiment.subsets", envir=experimentTibblesEnv)
  print(experiment.subsets)
}

#' Add any result to the results tibble of the experiment
#'
#' @title Add to Results
#' @param result The result
#' @param name Name of the result
#' @param type The type of the results (e.g., Correlation, t-test, ANOVA)
#' @param id Result id (optional)
#' @return The result
#' @export
results.add <- function (result, name, type = "", id = 0) {
  experiment.results <- get("experiment.results", envir=experimentTibblesEnv)
  result_list = list(result)
  if (id > 0 && any(with(experiment.results, ID == id))) {
    experiment.results <- mutate(
      experiment.results,
      Type = ifelse(ID == id, type, Type),
      Result = ifelse(ID == id, result_list, Result))
  } else if (any(with(experiment.results, Name == name))) {
    experiment.results <- mutate(
      experiment.results,
      Type = ifelse(Name == name, type, Type),
      Result = ifelse(Name == name, result_list, Result))
  } else {
    if (id == 0) {
      if (nrow(experiment.results) > 0) { id <- max(experiment.results$ID) + 1 } else { id <- 1 }
    }
    experiment.results <- add_row(
      experiment.results,
      ID = id,
      Name = name,
      Type = type,
      Result = result_list
    )
  }
  assign("experiment.results", experiment.results, envir=experimentTibblesEnv)
  return(result)
}

#' Get a result from the experiments' results tibble
#'
#' @title Get Result
#' @param id The result id or name
#' @return The result
#' @export
results.get <- function (id) {
  experiment.results <- get("experiment.results", envir=experimentTibblesEnv)
  if (is.numeric(id)) {
    return((filter(experiment.results, ID == id))$Result[[1]])
  } else {
    return((filter(experiment.results, Name == id))$Result[[1]])
  }
}

#' Lists all the stored results
#'
#' @title List Results
#' @export
results.list <- function () {
  experiment.results <- get("experiment.results", envir=experimentTibblesEnv)
  print(experiment.results)
}

#' Add a plot to the experiments' plots tibble
#'
#' @title Add to Plots
#' @param plot The plot
#' @param name Name of the plot
#' @param id Plot id (optional)
#' @return The plot
#' @export
plots.add <- function (plot, name, id = 0) {
  experiment.plots <- get("experiment.plots", envir=experimentTibblesEnv)
  plot_list = list(plot)
  if (id > 0 && any(with(experiment.plots, ID == id))) {
    experiment.plots <- mutate(
      experiment.plots,
      Plot = ifelse(ID == id, plot_list, Plot))
  } else if (any(with(experiment.plots, Name == name))) {
    experiment.plots <- mutate(
      experiment.plots,
      Plot = ifelse(Name == name, plot_list, Plot))
  } else {
    if (id == 0) {
      if (nrow(experiment.plots) > 0) { id <- max(experiment.plots$ID) + 1 } else { id <- 1 }
    }
    experiment.plots <- add_row(
      experiment.plots,
      ID = id,
      Name = name,
      Plot = plot_list
    )
  }
  assign("experiment.plots", experiment.plots, envir=experimentTibblesEnv)
  return(plot)
}

#' Get a plot from the experiments' plots tibble
#'
#' @title Get Plot
#' @param id The plot id or name
#' @return The plot
#' @export
plots.get <- function (id) {
  experiment.plots <- get("experiment.plots", envir=experimentTibblesEnv)
  if (is.numeric(id)) {
    return((filter(experiment.plots, ID == id))$Plot[[1]])
  } else {
    return((filter(experiment.plots, Name == id))$Plot[[1]])
  }
}

#' Lists all the stored plots
#'
#' @title List Plots
#' @export
plots.list <- function () {
  experiment.plots <- get("experiment.plots", envir=experimentTibblesEnv)
  print(experiment.plots)
}