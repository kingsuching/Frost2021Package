#' This function scrapes metadata attributes from the Harvard Dataverse metadata repository.
#' @import rvest
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @param url the website url
#' @return dataframe with metadata attributes
#' @export

scrape_harvard <- function(url) {
  name <- scrape_rvest(url, "#title")
  cols <- scrape_rvest(url, "th , #metrics-heading")
  data <- scrape_rvest(url, ".metrics-count-block , td")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  df$Citation <- scrape_rvest(url, ".citation-select")
  names(df) <- cols
  names(df)[6] <- "function"
  names(df)[5] <- "File Data"
  file_info_cols <- c("Name", "Downloads", "Variables", "Observations")
  file_info_names <- scrape_rvest(url, ".fileNameOriginal a")
  file_info_names <- file_info_names[file_info_names != ""]
  file_info <- data.frame(matrix(ncol = length(file_info_cols), nrow = length(file_info_names)))
  names(file_info) <- file_info_cols
  file_info <- cbind(file_info, file_info_names)
  file_info$Name <- file_info$file_info_names
  file_info <- file_info[, names(file_info) != "file_info_names"]
  downloads <- scrape_rvest(url, ".visible-lg-inline")
  variables <- scrape_rvest(url, ".unf-block span:nth-child(1)")
  observations <- scrape_rvest(url, ".unf-block span:nth-child(2)")
  if(!identical(downloads, character(0))) {
    while(length(downloads) != nrow(file_info)) {
      downloads <- append(downloads, 0)
    }
    file_info$Downloads <- parse_number(downloads)
  }else{
    file_info$Downloads <- NA
  }
  if(!identical(variables, character(0))) {
    while(length(variables) != nrow(file_info)) {
      variables <- append(variables, 0)
    }
    file_info$Variables <- parse_number(variables)
  }else{
    file_info$Variables <- NA
  }
  if(!identical(observations, character(0))) {
    while(length(observations) != nrow(file_info)) {
      observations <- append(observations, 0)
    }
    file_info$Observations <- parse_number(observations)
  }else{
    file_info$Observations <- NA
  }
  file_info <- nest(file_info, data = everything())
  df$`File Data` <- file_info
  return(df)
}
