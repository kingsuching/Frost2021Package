#' This function scrapes metadata attributes from the data.gov repository.
#' @import rvest
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @param url the website url
#' @return dataframe with metadata attributes
#' @export

scrape_datagov <- function(url) {
  cols <- c(Frost2021Package::scrape_rvest(url, ".module-heading , .table-toggle-less tr+ tr .dataset-label , #access-use h3"), scrape_rvest(url, ".dataset-label"))
  data <- c(scrape_rvest(url, ".module-narrow a"), scrape_rvest(url, ".table-toggle-less a , .dataset-details , td > span , #sec-dates td"))
  data
  cols <- cols %>% data.frame() %>% distinct()
  cols <- cols$.
  data <- data %>% data.frame() %>% rowid_to_column() %>% filter(rowid > 2)
  data <- data$.
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  names(df) <- cols
  social <- scrape_rvest(url, ".social a")
  string <- ""
  for(i in social) {
    string <- paste(string, i, sep = " ")
  }
  string <- str_trim(string)
  df$`Share on Social Sites` <- string
  publisher <- scrape_rvest(url, "tr:nth-child(4) span")
  publisher <- publisher[publisher != ""]
  df$Publisher <- publisher
  df$Contact <- scrape_rvest(url, ".contact a")
  topics <- scrape_rvest(url, ".topics a")
  topics <- topics %>% data.frame() %>% distinct()
  topics <- topics$.
  topics <- paste(topics, collapse = ", ")
  df$Topics <- topics
  df$`Terms of Use` <- scrape_rvest(url, ".terms a")[1]
  access <- scrape_rvest(url, "#access-use strong , .access-public a")
  access <- data.frame(access) %>% distinct()
  access <- access$access
  access <- str_remove_all(access, ":")
  access <- paste(access, collapse = ", ")
  df$`Access & Use Information` <- access
  return(df)
}
