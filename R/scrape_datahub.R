#' Extracts metadata attributes from Datahub.io
#' @import rvest
#' @import magrittr
#' @param url, the website url (must be linked to datahub.io)
#' @return dataframe of metadata attributes
#' @export

scrape_datahub <- function(url) {
  columns <- url %>%
    read_html() %>%
    html_nodes(".no-left-padding th:nth-child(6) , .no-left-padding th:nth-child(5) , .no-left-padding th:nth-child(4) , .no-left-padding th:nth-child(3) , .no-left-padding th:nth-child(2) , .no-left-padding th:nth-child(1)") %>%
    html_text()
  df <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(df) <- columns
  data <- url %>%
    read_html() %>%
    html_nodes(".format-list~ .col-xs-2+ .col-xs-2 , .format-list , .no-left-padding .col-xs-1")  %>%
    html_text()
  data <- data %>%
    str_remove_all("\n")
  data <- rbind(df, columns) %>%
    rbind(data) %>%
    rowid_to_column() %>%
    filter(rowid > 1)
  data <- target(data, c("Name", "X.Files.", "X.Size.", "X.Format.", "X.Created.", "X.Updated.", "X.Licence."))
  data$X.Size. <- toupper(data$X.Size.) %>% convert() %>% checkNull()
  name <- paste(scrape_rvest(url, "h1"), collapse = " ")
  name <- str_remove_all(name, " Certified")
  name <- paste(name, collapse = ", ")
  name <- str_trim(name)
  data$Name <- checkNull(name)
  return(data)
}
