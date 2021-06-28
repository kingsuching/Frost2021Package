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
  df <- mutate(df, Name = scrape_rvest(url, ".prose h1"), Tags = scrape_rvest(url, ".well"))
  return(df)
}
