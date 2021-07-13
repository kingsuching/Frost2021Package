#' This function scrapes metadata attributes from the Zenodo data repository.
#' @import rvest
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @param url the website url
#' @return dataframe with metadata attributes
#' @export

scrape_zenodo <- function(url) {
  name <- scrape_rvest(url, "h1")
  author <- scrape_rvest(url, "h1 + p")
  cols <- c(c("Name", "Authors"), scrape_rvest(url, ".addthis_32x32_style+ h4 , dt:nth-child(14) , dt:nth-child(12) , dt:nth-child(5) , dt:nth-child(1) , #collapse-stats tr:nth-child(6) td:nth-child(1) , #collapse-stats tr:nth-child(5) td:nth-child(1)"))
  data <- scrape_rvest(url, "dd:nth-child(15) a , dd:nth-child(13) li , dd:nth-child(2) , #collapse-stats tr:nth-child(6) td:nth-child(2) , #collapse-stats tr:nth-child(5) td:nth-child(2) , h1+ p , h1 , #invenio-csl .ng-binding")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  keyword <- scrape_rvest(url, "dd:nth-child(6)")
  communities <- scrape_rvest(url, "dd:nth-child(13) li")
  file_info <- data.frame(Name = scrape_rvest(url, ".filename"), SizeMB = scrape_rvest(url, ".nowrap:nth-child(2)"))
  numbers <- parse_number(file_info$SizeMB)
  count <- 1
  for(i in file_info$SizeMB) {
    if("kB" %in% i) {
      numbers[count] <- numbers[count]/1000
    }else if("GB" %in% i) {
      numbers[count] <- numbers[count]*1000
    }
  }
  file_info$SizeMB <- numbers
  file_info <- file_info %>% nest(data = everything())
  df <- mutate(df, FileInfo = file_info)
  data <- append(data, file_info)
  cols <- append(cols, "File Info")
  names(df) <- cols[1:length(data)]
  if(!identical(keyword, character(0)) & !str_detect(keyword, "Supplement")) {
    df$`Keyword(s):` <- keyword %>% str_trim()
  }else{
    df$`Keyword(s):` <- c(NA)
  }
  if(!identical(communities, character(0))) {
    df$`Communities:` <- paste(communities, collapse = ", ")
  }else{
    df$`Communities:` <- c(NA)
  }
  license <- paste(scrape_rvest(url, "dd:nth-child(15) a"), collapse = ", ")
  if(!identical(license, character(0))) {
    df$`License (for files):` <- license
  }else{
    df$`License (for files):` <- c(NA)
  }
  citation <- scrape_rvest(url, ".ng-binding")
  if(!identical(citation, character(0))) {
    df$`Cite as` <- scrape_rvest(url, ".ng-binding")
  }else{
    citation <- scrape_rvest(url, "#invenio-csl p")
    if(!identical(citation, character(0))) {
      df$`Cite as` <- citation
    }else{
      df$`Cite as` <- c(NA)
    }
  }
  df <- mutate(df, FileInfo <- file_info)
  file_info <- unnest(file_info, cols = c(data))
  df <- mutate(df, FileName = paste(file_info$Name, collapse = ", "), SizeMB = paste(file_info$SizeMB, collapse = ", "))
  df <- df[, c("Name", "Authors", "Unique views", "Unique downloads", "Publication date:", "Keyword(s):", "Communities:", "FileName", "SizeMB")]
  return(df)
}
