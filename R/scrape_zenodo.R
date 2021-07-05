#' This function scrapes metadata attributes from the Harvard Dataverse metadata repository.
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
  if(!identical(keyword, character(0))) {
    keyword <- paste(keyword, collapse = ", ")
    df$`Keyword(s):` <- keyword
  }else{
    df$`Keyword(s):` <- c(NA)
  }
  communities <- scrape_rvest(url, "dd:nth-child(13) li")
  if(!identical(communities, character(0))) {
    communities <- paste(communities, collapse = ", ")
    df$`Communities:` <- communities
  }else{
    df$`Communities:` <- c(NA)
  }
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
  df$`File Info` <- file_info
  names(df) <- cols
  return(df)
}
