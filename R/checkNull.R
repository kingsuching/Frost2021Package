#' This function checks if a variable is equivalent to character(0) and handles the data accordingly.
#' @param item the item to be checked
#' @return NA if the item is equivalent to character(0), the item otherwise
#' @export

checkNull <- function(item) {
  if(!identical(item, character(0))) {
    return(item)
  }
  return(c(NA))
}
