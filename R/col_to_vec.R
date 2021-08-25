#' This converts a data frame column to a vector, similar to the "$" operator in base R.
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import purrr
#' @param col, the data frame column
#' @return vector with elements of the column
#' @export

col_to_vec <- function(col) {
  if(ncol(col) != 1) {
    stop(paste("Need exactly one column. There are", ncol(col), "columns"))
  }
  vector <- c()
  for(i in 1:nrow(col)) {
    vector <- append(vector, col[i, names(col)[1]])
  }
  return()
}
