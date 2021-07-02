#' This function scrapes metadata (date uploaded, viewing metrics, etc.) from the Data Dryad repository.
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @param url, the website url
#' @return dataframe with scraped metadata contents from Data Dryad
#' @export

scrape_dryad <- function(url) {
  cols <- scrape_rvest(url, ".c-sidebox__heading , .o-heading__level2")
  data <- scrape_rvest(url, ".c-locations__data , .o-heading__level2+ p , .t-landing__text-wall p , #show_license p , .o-metrics__metric , .c-file-group , .c-file-group__summary")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  file_info_columns <- scrape_rvest(url, "#sidebar a")
  file_info_data <- scrape_rvest(url, ".c-file-group__list div")
  if(!identical(file_info_data, character(0))) {
    file_info <- data.frame(filename = file_info_columns, sizeMB = file_info_data)
  }else{
    file_info <- c(NA)
  }
  if(!is.na(file_info)) {
    numbers <- parse_number(file_info$sizeMB)
    count <- 1
    for(i in file_info$sizeMB) {
      if("k" %in% i) { # if reported in kilobytes
        numbers[count] <- numbers[count]/1000
      }else if(!("MB" %in% i)) { # if reported in bytes
        numbers[count] <- numbers[count]/1000000
      }
      count <- count+1
    }
    file_info$sizeMB <- numbers
    file_info <- nest(file_info, data = everything())
  }
  names(df) <- cols[1:length(data)]
  df$`Data Files` <- file_info
  methods <- scrape_rvest(url, ".t-landing__text-wall:nth-child(8) p:nth-child(1)")
  if(!identical(methods, character(0))) {
    df$Methods <- methods
  }else{
    df$Methods <- c(NA)
  }
  authors <- paste(scrape_rvest(url, ".o-metadata__author"), collapse = "; ")
  if(!identical(authors, character(0))) {
    df$Authors <- authors
  }else{
    df$Authors <- c(NA)
  }
  return(df[, c("Citation", "Abstract", "Methods", "Data Files", "Authors")])
}
