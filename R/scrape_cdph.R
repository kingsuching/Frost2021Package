#' This function scrapes metadata attributes from the CDC metadata repository.
#' @import rvest
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @param url the website url
#' @return dataframe with metadata attributes
#' @export

scrape_cdph <- function(url) {
  cols <- scrape_rvest(url, ".module-heading , dt , .dataset-label , .tags h3")
  data <- scrape_rvest(url, ".license span , .nav-item a , .module-shallow .heading , dd , .dataset-details , .well")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  names(df) <- cols
  social <- scrape_rvest(url, ".nav-item a")
  df$Social <- paste(social, collapse = ", ")
  return(df)
}
