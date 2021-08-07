#' This function converts the file size to MB.
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @param x the file size in string and unit format
#' @return the file size in MB
#' @export

convert <- function(x) {
  if(is.na(x)) {
    return(NA)
  }
  num <- parse_number(x)
  if(str_detect(x, "GB")) {
    num <- num*1000
  }else if(str_detect(x, "KB")) {
    num <- num/1000
  }else if(str_detect(x, "[B]{1}[:print:]{0}")) {
    num <- num/(10^6)
  }
  return(num)
}
