#' This function scrapes metadata attributes from the Figshare metadata repository.
#' @import rvest
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @param url the website url
#' @return dataframe with metadata attributes
#' @export

scrape_figshare <- function(url) {
  name <- scrape_rvest(url, "._3lGK4")
  description <- scrape_rvest(url, "._1dO13")
  cols <- scrape_rvest(url, "._36trp , ._14z3R , ._1qu0d+ span")
  categories <- scrape_rvest(url, "li")
  keywords <- scrape_rvest(url, "._3v5nv span")
  date <- scrape_rvest(url, "._1qu0d")
  date <- checkNull(date)
  exports <- scrape_rvest(url, "button span")
  exports <- exports[exports != "Select an option"]
  df <- data.frame(Name = name, Description = description, Categories = paste(categories, collapse = ", "), Keywords = paste(keywords, collapse = ", "), Date = date, Exports = paste(exports, collapse = ", "))
  return(df)
}
