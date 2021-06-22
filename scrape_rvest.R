#' @author Sucheen Sundaram
#' Date 6/21/2021
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @param url the website url
#' @param type the html formatting type to extract the correct items
#' @export scrape_rvest

scrape_rvest <- function(url, type) {
  html_fm <- rvest::read_html(url) # Reads html script from given URL
  # After reading html script, extract the text by obtaining the html notes with the given html specifier (param type)
  extraction <- html_fm %>% rvest::html_nodes(type) %>% html_text() %>%
    str_remove_all("\n") %>%
    # Clean up the string with scraped data
    str_trim()
  return(extraction)
}
