#' This function drops the rowid columns
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import purrr
#' @param df, data frame
#' @return df without rowid columns
#' @export

drop_rowid <- function(df) {
  rowid_cols <- c()
  for(i in names(df)) {
    if(vec_eq(col_to_vec(df[i]), c(1:nrow(df)))) {
      rowid_cols <- append(rowid_cols, i)
    }
  }
  if(vec_eq(rowid_cols, c())) {
    rowid_base <- str_extract(names(df), "X[.]*[:digit:]+")
    rowid_base <- rowid_base[!is.na(rowid_base)]
    return(df[!(names(df) %in% rowid_base)])
  }
  return(df[!(names(df) %in% rowid_cols)])
}
