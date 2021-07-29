#' This function builds the metadata dataframe from a given list of data frames.
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @import lubridate
#' @param arr, list of data frames
#' @return complete metadata dataframe
#' @export

build_mdf <- function(arr) {
  full_names <- c()
  for(i in arr) {
    full_names <- c(full_names, names(i))
  }
  mdf <- data.frame(matrix(ncol = length(full_names), nrow = 0))
  names(mdf) <- full_names
  for(i in arr) {
    mdf <- rbind(mdf, target(i, names(mdf)))
  }
  return(mdf)
}
