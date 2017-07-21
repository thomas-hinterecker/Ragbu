#' Indicate columns of a data.frame as factors
#'
#' @param data        The data frame
#' @param tofactor    A vector indicating the column names of the column which should be factorized.
#' @return data.frame
#' @export
as.factors <- function (data, tofactor) {
  data <- data %>% {.[,tofactor] <- lapply(.[,tofactor], as.factor); return(.)}
  return(data)
}