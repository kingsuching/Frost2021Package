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
  file_info_cols <- c("FileName", "Size")
  file_info_data <- checkNull(scrape_rvest(url, "._1KU5g span"))
  df <- data.frame(Name = checkNull(name), Description = checkNull(description), Categories = checkNull(paste(categories, collapse = ", ")), Keywords = checkNull(paste(keywords, collapse = ", ")), Date = checkNull(date), Exports = checkNull(paste(exports, collapse = ", ")))
  count <- 1
  for(i in file_info_cols) {
    df[i] <- checkNull(file_info_data[count])
    count <- count+1
  }
  return(df)
}
