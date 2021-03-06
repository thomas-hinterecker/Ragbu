#' Indicate columns of a data frame as factors
#'
#' @title Factorize variables
#' @param data     The data frame
#' @param tofactor A vector indicating the column names of the column which should be factorized.
#' @return Data frame
#' @export
as.factors <- function (data, tofactor) {
  data[,tofactor] <- lapply(data[,tofactor], as.factor)
  return(data)
}