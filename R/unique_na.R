#' This returns distinct values of the elements in a vector, including NA values.
#' @import rvest
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import purrr
#' @param df data frame
#' @param column the column
#' @return the distinct values of the column
#' @export

unique_na <- function(df, column) {
  df <- df %>%
    rowid_to_column()
  vals <- c()
  rows <- c()
  for(i in 1:nrow(df)) {
    if(!(df[i, column] %in% vals) | is.na(df[i, column])) {
      vals <- append(vals, df[i, column])
      rows <- append(rows, i)
    }
  }
  return(df[rows, names(df) != "rowid"])
}
