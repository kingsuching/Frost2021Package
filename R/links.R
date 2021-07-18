#' This function scrapes data links according to a given CSS selector.
#' @import rvest
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @param url the website url
#' @return vector of links
#' @export

links <- function(url, css) {
  pg <- read_html(url)
  lnk <- html_attr(html_nodes(pg, css), "href")
  return(lnk)
}
