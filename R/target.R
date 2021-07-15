#' This function modifies the dataframe based on a set of target columns.
#' It will replace a column with NA if it is not present in the dataframe and only
#' return those columns.
#' @param df, data frame
#' @param target, target columns
#' @return data frame modified around the target columns as specified
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @export

target <- function(df, target_cols) {
  for(i in target_cols) {
    if(!(i %in% names(df))) {
      df[i] <- NA
    }
  }
  return(df[target_cols])
}
