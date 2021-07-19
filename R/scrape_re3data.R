#' This function does some more scraping.
#' @import dplyr
#' @import magrittr
#' @import Frost2021Package
#' @import stringr
#' @import rvest
#' @import DT
#'
#' @export

scrape_re3data <- function(url) {
  cols <- scrape_rvest(url, ".col-sm-3")[1:13]
  data <- scrape_rvest(url, ".col-sm-9")[1:13]
  df <- cbind(cols, data) %>% data.frame() %>% pivot_wider(names_from = cols, values_from = data)
  df$`Subject(s)` <- scrape_rvest(url, ".subjects") %>% paste(collapse = ", ")
  inst_cols <- scrape_rvest(url, ".content-block:nth-child(1) .col-sm-12:nth-child(11) .col-sm-3 , .content-block:nth-child(1) .col-sm-12:nth-child(9) .col-sm-3 , .content-block:nth-child(1) .col-sm-12:nth-child(7) .col-sm-3 , .content-block:nth-child(1) .col-sm-12:nth-child(1) .col-sm-3")[1:4]
  inst_cols <- inst_cols[inst_cols != "Type(s) of responsibility"]
  inst_data <- scrape_rvest(url, ".content-block:nth-child(1) .col-sm-12:nth-child(11) .col-sm-9 , .content-block:nth-child(1) .country , .content-block:nth-child(1) .col-sm-12:nth-child(1) .col-sm-9")
  df$Responsibility <- paste(checkNull(scrape_rvest(url, ".content-block:nth-child(1) .col-sm-12:nth-child(9) li")), collapse = ", ")
  df <- c(df, inst_data)
  df <- df %>% data.frame()
  return(df[,1:10])
}
