#' This function scrapes metadata (date uploaded, viewing metrics, etc.) from the Data Dryad repository.
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @param url, the website url
#' @return dataframe with scraped metadata contents from Data Dryad
#' @export

scrape_dryad <- function(url) {
  cols <- scrape_rvest(url, ".c-sidebox__heading , .o-heading__level2 , .o-heading__level1")
  data <- scrape_rvest(url, ".c-locations__data , .o-heading__level2+ p , .t-landing__text-wall p , .o-metadata__author , #show_license p , .o-metrics__metric , .c-file-group , .c-file-group__summary")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  names(df) <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  df <- df %>%
    mutate(Authors = paste(one, two, three, sep = "; "))
  df <- df[!(names(df) %in% c("one", "two", "three"))]
  file_info_columns <- scrape_rvest(url, "#sidebar a")
  file_info_data <- scrape_rvest(url, ".c-file-group__list div")
  file_info <- data.frame(filename = file_info_columns, sizeMB = file_info_data)
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
  df$eight <- file_info %>%
    nest(data = everything())
  pieces <- map_int(names(df), ~str_split(df[.x], " ") %>% pluck(1) %>% length())
  df <- df[pieces != 2]
  names(df) <- c("Citation", "Abstract", "Methods", "File Information", "Upload Date", "Authors")
  authors <- str_split(df$Citation, " [(]") %>% pluck(1)
  df <- mutate(df, Authors = authors[1])
  return(df)
}
