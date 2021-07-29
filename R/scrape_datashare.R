#' This function scrapes metadata attributes from the edinburgh datashare repository.
#' @import rvest
#' @import magrittr
#' @import stringr
#' @import dplyr
#' @import purrr
#' @param url, the url
#' @return dataframe with metadata attributes
#' @export

scrape_datashare <- function(url) {
  cols <- c("Name", scrape_rvest(url, ".word-break h5"))
  stats_url <- paste(url, "/statistics", sep = "")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  data <- scrape_rvest(url, "#aspect_artifactbrowser_ItemViewer_div_item-view .word-break , .first-page-header")
  df <- rbind(df, data)
  names(df) <- cols
  for(i in names(df)) {
    df[i] <- str_remove_all(df[i], i)
  }
  df$Views <- scrape_rvest(stats_url, "#aspect_statistics_StatisticsTransformer_div_stats > .table-responsive:nth-child(2) .datacell") %>%
    parse_number() %>%
    sum()
  df$`Top Country` <- checkNull(scrape_rvest(stats_url, ".table-responsive:nth-child(7) #aspect_statistics_StatisticsTransformer_cell_01"))
  files <- scrape_rvest(url, ".col-sm-8 a")
  files <- files[files != "" & !is.na(files)]
  filename <- str_extract(files, "[:print:]+[(]{1}")
  filename <- str_remove_all(filename, "[ (]")
  filesize <- str_extract(files, "[(][:print:]+[)]")
  df$filename <- paste(filename, collapse = "; ")
  numbers <- parse_number(filesize)
  count <- 1
  for(i in numbers) {
    if(str_detect(i, "Kb")) {
      numbers[count] <- numbers[count]/1000
    }
    count <- count+1
  }
  df$sizeMB <- paste(parse_number(filesize), collapse = "; ")
  df$Author <- checkNull(scrape_rvest(url, ".simple-item-view-creators")) %>%
    str_remove_all("Creator")
  return(target(df, c("Name", "Author", "Date Available", "Description", "Top Country", "Citation", "Views", "filename", "filesize", "Type")))
}
