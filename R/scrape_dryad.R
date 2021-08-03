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
    file_info <- NA
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
    df$sizeMB <- paste(file_info$sizeMB, collapse = "; ")
    df$filename <- paste(file_info$filename, collapse = "; ")
  }
  names(df) <- cols[1:length(data)]
  methods <- scrape_rvest(url, ".t-landing__text-wall:nth-child(8) p:nth-child(1)")
  if(!identical(methods, character(0))) {
    df$Methods <- paste(methods, collapse = ", ")
  }else{
    df$Methods <- c(NA)
  }
  authors <- paste(scrape_rvest(url, ".o-metadata__author"), collapse = "; ")
  if(!identical(authors, character(0))) {
    df$Authors <- authors
  }else{
    df$Authors <- c(NA)
  }
  date <- scrape_rvest(url, ".c-file-group__summary")
  if(identical(date, character(0))) {
    date <- scrape_rvest(url, ".o-metadata__group2-item:nth-child(1)")
    date <- str_split(date, ": ") %>% pluck(1)
    date <- date[2]
  }
  if(!identical(date, character(0))) {
    if(length(date) == 1) {
      df$Date <- date
    }else{
      df$Date <- date[1]
    }
  }else{
    df$Date <- c(NA)
  }
  affiliation <- scrape_rvest(url, ".o-metadata__affiliation")
  if(!identical(affiliation, character(0))) {
    df$AuthorAffiliation <- paste(affiliation, collapse = ", ")
  }else{
    df$AuthorAffiliation <- c(NA)
  }
  df$Name <- scrape_rvest(url, ".o-heading__level1") %>% checkNull()
  df <- df[!is.na(names(df))]
  if(!is.na(file_info)) {
    df$sizeMB <- paste(file_info$sizeMB, collapse = "; ")
    df$filename <- paste(file_info$filename, collapse = "; ")
  }
  df$Abstract <- checkNull(scrape_rvest(url, ".t-landing__text-wall"))[1]
  targets <- c("Name", "Citation", "Abstract", "Methods", "filename", "sizeMB", "Authors", "AuthorAffiliation", "Date", "Publication Date")
  doi <- str_split(url, "doi:") %>% pluck(1)
  doi <- doi[2]
  tryCatch(expr = {
    doi_df <- dryad_dataset(doi) %>%
      data.frame()
    df$`Publication Date` <- doi_df$`X10.5061.dryad.tqjq2bvxm.publicationDate`
  }, error = function(error) {
    df$`Publication Date` <- c(NA)
  })
  return(target(df, targets))
}
