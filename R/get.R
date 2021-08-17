#' Optimizes vector indexing for the magrittr package.
#' @import rvest
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import purrr
#' @import lubridate
#' @param vec vector of elements
#' @param index the vector index
#' @return element at the index
#' @export

get <- function(vec, index) {
  return(vec[index])
}
