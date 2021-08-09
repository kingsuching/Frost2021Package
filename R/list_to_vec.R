#' This function converts a list to a vector.
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @param l, the list
#' @return vector with the elements of the list
#' @export

list_to_vec <- function(l) {
  v <- c()
  for(i in 1:length(l)) {
    v <- append(v, l[[i]])
  }
  return(v)
}
