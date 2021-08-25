#' This function checks if two vectors are equal, that is, if they are the same length and their elements
#' are in the same order.
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @import stringr
#' @param a, vector a
#' @param b, vector b
#' @return whether or not a and b are equal according to the above definition
#' @export

vec_eq <- function(a, b) {
  if(length(a) != length(b)) {
    return(FALSE)
  }
  return(!(FALSE %in% (a == b)))
}
