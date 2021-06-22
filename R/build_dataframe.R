#' Builds the Dataframe
#' @import rvest
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @param url, url to be scraped
#' @param css_selectors, vector of css_selectors to scrape
#' @return dataframe with scraped data organized into columns by their CSS selector tag. May require postprocessing.
#'
#' @export

build_dataframe <- function(url, css_selectors) {
  scraped <- data.frame(css_selectors)
  for(i in names(scraped)) {
    scraped$i <- scrape_rvest(url, css_selectors[i])
  }
  return(scraped)
}
