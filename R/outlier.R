#' Removes outliers in a given data frame. Outliers can be remove within or across subjects.
#'
#' @title Remove Outliers in Data Frame
#' @param data              The data frame
#' @param columns           Dependent variables
#' @param within_subjects   Remove outlier within subjects or overall subjects?
#' @param wid               Subject identifier variable
#' @param median            Should the outliers be identified via standard deviation or via median (boxplot)?
#' @param sd_times          Below/above how many standard deviations from the mean is a value considered as an outlier?
#' @param print             Should the proportion of outliers in the data be printed?
#' @return Data frame with removed outliers (indicated as NA)
#' @export
rm.outlier <- function (data, columns, within_subjects = FALSE, wid = NULL, median = FALSE, sd_times = 3, print = FALSE) {
  rm.outlier.do <- function (x, median = FALSE, sd_times = 3) {
    if (median == FALSE) {
      upper <- mean(x, na.rm=TRUE)+sd(x, na.rm=TRUE)*sd_times
      lower <- mean(x, na.rm=TRUE)-sd(x, na.rm=TRUE)*sd_times
      x[x<lower] <- NA
      x[x>upper] <- NA
    } else {
      l <- sort(boxplot.stats(x)$out)
      x[x >= l[1]] <- NA
    }
    return(x)
  }
  ncols <- length(columns)
  if (ncols<1) {
    print("No column set")
  }  else {
    if (within_subjects==FALSE) {
      for (i in 1:ncols) {
        data[,columns[i]] <- rm.outlier.do(unlist(data[,columns[i]]), median, sd_times)
      }
    } else {
      if (is.null(wid)==TRUE) {
        print("wid is not set")
      }
      ss <- unlist(unique(data[,wid]))
      for (z in 1:length(ss)) {
        for (i in 1:ncols) {
          data[data[,wid]==as.character(ss[z]),columns[i]] <- rm.outlier.do(unlist(data[data[,wid]==as.character(ss[z]),columns[i]]), median, sd_times)
        }
      }
    }
    if (print==TRUE) {
      for (i in 1:ncols) {
        print(paste("Outliers in ", columns[i], ": ", round(length(unlist(data[is.na(data[,columns[i]]),columns[i]]))/length(unlist(data[,columns[i]])), 3)*100, " %", sep=""))
      }
    }
  }
  return(data)
}