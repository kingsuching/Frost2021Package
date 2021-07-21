#' This function perfoms the merge-and-drop technique for condensing the metadata dataframe as discussed at a meeting. Shifts from col2 to col1.
#' @import rvest
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import purrr
#' @param df, data frame
#' @param col1, the first column
#' @param col2, the second column
#' @return merged dataframe
#' @export

merge_and_drop <- function(df, col1, col2) {
  for(i in 1:nrow(df)) {
    if(!is.na(df[i, col2]) & is.na(df[i, col1])) {
      df[i, col1] <- df[i, col2]
    }
  }
  if(col1 != col2) {
    df <- df[names(df) != col2]
  }
  return(df)
}
