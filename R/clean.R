#' This function cleans the data and removes unnecessary columns
#' @import dplyr
#' @import magrittr
#' @param df, data frame
#' @return cleaned dataset
#' @export

clean <- function(df) {
  cols <- c()
  for(i in names(df)) {
    if((df[i] %>% drop_na() %>% nrow()) != 0) {
      cols <- append(cols, i)
    }
  }
  return(df[cols])
}
