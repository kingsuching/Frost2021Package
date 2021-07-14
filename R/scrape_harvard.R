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
  cols <- scrape_rvest(url, "#metrics-heading , th")
  data <- scrape_rvest(url, ".metrics-count-block , td")
  data <- data[data != "" & !str_detect(data, "dataDictionary")]
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  df <- rbind(df, data)
  names(df) <- cols[1:ncol(df)]
  file_info_cols <- c("Name", "Downloads", "Variables", "Observations")
  file_info_names <- scrape_rvest(url, ".fileNameOriginal a")
  file_info_names <- file_info_names[file_info_names != ""]
  file_info <- data.frame(matrix(ncol = length(file_info_cols), nrow = length(file_info_names)+1))
  names(file_info) <- file_info_cols
  #file_info <- cbind(file_info, file_info_names)
  file_info$`FileName` <- checkNull(scrape_rvest(url, "#datasetForm\\:tabView\\:filesTable\\:0\\:fileInfoInclude-filesTable a"))
  fn <- data.frame(file_info$FileName) %>% distinct()
  fn <- fn$file_info.FileName
  file_info$FileName <- fn
  file_info <- file_info[, names(file_info) != "file_info_names"]
  downloads <- scrape_rvest(url, ".visible-lg-inline")
  variables <- scrape_rvest(url, ".unf-block span:nth-child(1)")
  observations <- scrape_rvest(url, ".unf-block span:nth-child(2)")
  if(!identical(downloads, character(0))) {
    while(length(downloads) != nrow(file_info)) {
      downloads <- append(downloads, NA)
    }
    file_info$Downloads <- parse_number(downloads)
  }else{
    file_info$Downloads <- NA
  }
  if(!identical(variables, character(0))) {
    while(length(variables) != nrow(file_info)) {
      variables <- append(variables, NA)
    }
    file_info$Variables <- parse_number(variables)
  }else{
    file_info$Variables <- NA
  }
  if(!identical(observations, character(0))) {
    while(length(observations) != nrow(file_info)) {
      observations <- append(observations, NA)
    }
    file_info$Observations <- parse_number(observations)
  }else{
    file_info$Observations <- NA
  }
  for(i in names(file_info)) {
    df[i] <- paste(c(file_info[i]), collapse = "; ")
  }
  df$Author <- checkNull(scrape_rvest(url, "#metadata_author td"))
  df$Name <- checkNull(name)
  df$`Deposit Date` <- checkNull(scrape_rvest(url, "#metadata_dateOfDeposit td"))
  df$Citation <- checkNull(scrape_rvest(url, ".citation-select"))
  target <- c("Name", "Description", "Subject", "Keyword", "Deposit Date", "Author", "Depositor", "Citation", "FileName", "Downloads", "Variables", "Observations")
  for(i in target) {
    if(!(i %in% names(df))) {
      df[i] <- c(NA)
    }
  }
  return(df[target])
}
